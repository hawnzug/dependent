{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Text as T
import Syntax
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Except

type Context = Map.Map Name (Expr, Maybe Expr)
type Runner a = ReaderT Context (Either T.Text) a

freeVars :: Expr -> Set Name
freeVars (Var name) = Set.singleton name
freeVars (Universe _) = Set.empty
freeVars (Pi (Abstraction n t e)) = freeVars t `Set.union` (Set.delete n $ freeVars e)
freeVars (Lambda (Abstraction n t e)) = freeVars t `Set.union` (Set.delete n $ freeVars e)
freeVars (App f a) = freeVars f `Set.union` freeVars a

subst :: Expr -> Name -> Expr -> Expr
subst (Var n) x e = if n == x then e else Var n
subst u@(Universe _) _ _ = u
subst (App f a) x e = App (subst f x e) (subst a x e)
subst (Pi abstr) x e = Pi (substAbs abstr x e)
subst (Lambda abstr) x e = Lambda (substAbs abstr x e)

substAbs :: Abstraction -> Name -> Expr -> Abstraction
substAbs abstr x e
  | n == x = abstr{typ=subst t x e}
  | n `elem` fvs =
      let n' = rename b n
          b' = subst b n (Var n')
       in abstr{bound=n', typ=subst t x e, body=subst b' x e}
  | otherwise = abstr{typ=subst t x e, body=subst b x e}
    where n = bound abstr
          t = typ abstr
          b = body abstr
          fvs = freeVars e
          rename e i = loop i
              where loop i' = if i' `elem` vars then loop (T.append i "'") else i'
                    vars = fvs `Set.union` freeVars e

nf :: Expr -> Runner Expr
nf (Var name) = do
  ctx <- ask
  maybe (return $ Var name) nf (Map.lookup name ctx >>= snd)
nf e@(Universe k) = return e
nf (App f a) = do
  e <- nf f
  case e of
    Lambda abstr -> do
      nfa <- nf a
      nf $ subst (body abstr) (bound abstr) nfa
    _ -> App e <$> nf a
nf (Lambda abstr) = Lambda <$> nfAbstr abstr
nf (Pi abstr) = Pi <$> nfAbstr abstr

nfAbstr :: Abstraction -> Runner Abstraction
nfAbstr abstr = do
  nt <- nf (typ abstr)
  nb <- local (Map.insert (bound abstr) (nt, Nothing)) $ nf (body abstr)
  return abstr{typ=nt, body=nb}

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var x) (Var y) = x == y
alphaEq (Universe x) (Universe y) = x == y
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lambda abs1) (Lambda abs2) = eqAbstr abs1 abs2
alphaEq (Pi abs1) (Pi abs2) = eqAbstr abs1 abs2
alphaEq _ _ = False

eqAbstr :: Abstraction -> Abstraction -> Bool
eqAbstr abs1 abs2 = alphaEq t1 t2 && alphaEq b1 (subst b2 n2 (Var n1))
    where b1 = body abs1
          b2 = body abs2
          n1 = bound abs1
          n2 = bound abs2
          t1 = typ abs1
          t2 = typ abs2

betaEq :: Expr -> Expr -> Runner Bool
betaEq e1 e2 = do
  ne1 <- nf e1
  ne2 <- nf e2
  return $ alphaEq ne1 ne2

infer :: Expr -> Runner Expr
infer (Var x) = do
  ctx <- ask
  case Map.lookup x ctx of
    Just (t, _) -> return t
    Nothing -> throwError "cannot find var"
infer (Universe k) = return $ Universe (k+1)
infer (Pi abstr) = do
  let t1 = typ abstr
  k1 <- inferUniverse t1
  k2 <- local (Map.insert (bound abstr) (t1, Nothing)) (inferUniverse $ body abstr)
  return $ Universe (max k1 k2)
infer (Lambda abstr) = do
  let n = bound abstr
      t = typ abstr
  tb <- local (Map.insert n (t, Nothing)) $ infer (body abstr)
  return $ Pi Abstraction{bound=n, typ=t, body=tb}
infer (App f a) = do
  Abstraction{bound=n, typ=t1, body=b} <- inferPi f
  t2 <- infer a
  eq <- betaEq t1 t2
  if eq
    then return $ subst b n a
    else throwError "function type error"

inferUniverse :: Expr -> Runner Int
inferUniverse t = do
  u <- infer t
  mu <- nf u
  case mu of
    Universe k -> return k
    _ -> throwError "type expected"

inferPi :: Expr -> Runner Abstraction
inferPi e = do
  t <- infer e
  mpi <- nf t
  case mpi of
    Pi a -> return a
    _ -> throwError "function expected"

emptyContext :: Context
emptyContext = Map.empty

addType :: Name -> Expr -> Context -> Context
addType n t = Map.insert n (t, Nothing)

addDef :: Name -> Expr -> Expr -> Context -> Context
addDef n t e = Map.insert n (t, Just e)

_betaEq :: Expr -> Expr -> Bool
_betaEq e1 e2 = either (const False) id $ runReaderT (betaEq e1 e2) emptyContext
