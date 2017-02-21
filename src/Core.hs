{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Text as T
import Syntax
import Data.List (union, (\\))
import qualified Data.Map.Strict as Map

type Context = Map.Map Name (Expr, Maybe Expr)

freeVars :: Expr -> [Name]
freeVars (Var name) = [name]
freeVars (Universe _) = []
freeVars (Pi abstr) = freeVars (typ abstr) `union` (freeVars (body abstr) \\ [bound abstr])
freeVars (Lambda abstr) = freeVars (typ abstr) `union` (freeVars (body abstr) \\ [bound abstr])
freeVars (App f a) = freeVars f `union` freeVars a

subst :: Expr -> Name -> Expr -> Expr
subst (Var n) x e = if n == x then e else Var n
subst e@(Universe _) _ _ = e
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
                    vars = fvs `union` freeVars e

nf :: Context -> Expr -> Expr
nf ctx (Var x) = case Map.lookup x ctx of
                   Just (_, Just e) -> nf ctx e
                   _ -> Var x
nf ctx e@(Universe k) = e
nf ctx (App f a) = case nf ctx f of
                     Lambda abstr -> nf ctx (subst (body abstr) (bound abstr) (nf ctx a))
                     e -> App e (nf ctx a)
nf ctx (Lambda abstr) = Lambda (nfAbstr ctx abstr)
nf ctx (Pi abstr) = Pi (nfAbstr ctx abstr)

nfAbstr :: Context -> Abstraction -> Abstraction
nfAbstr ctx abstr = abstr{typ=nt, body=nb}
    where nt = nf ctx (typ abstr)
          nb = nf (Map.insert (bound abstr) (nt, Nothing) ctx) (body abstr)

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

betaEq :: Context -> Expr -> Expr -> Bool
betaEq ctx e1 e2 = alphaEq (nf ctx e1) (nf ctx e2)

infer :: Context -> Expr -> Either T.Text Expr
infer ctx (Var x) = case Map.lookup x ctx of
                      Just (t, _) -> Right t
                      Nothing -> Left "cannot find var"
infer ctx (Universe k) = return $ Universe (k+1)
infer ctx (Pi abstr) = do
    k1 <- inferUniverse ctx t1
    k2 <- inferUniverse (Map.insert (bound abstr) (t1, Nothing) ctx) (body abstr)
    return $ Universe (max k1 k2)
        where t1 = typ abstr
infer ctx (Lambda abstr) = do
    tb <- infer (Map.insert n (t, Nothing) ctx) (body abstr)
    return $ Pi Abstraction{bound=n, typ=t, body=tb}
        where n = bound abstr
              t = typ abstr
infer ctx (App f a) = do
    Abstraction{bound=n, typ=t1, body=b} <- inferPi ctx f
    t2 <- infer ctx a
    if betaEq ctx t1 t2
       then return $ subst b n a
       else Left "function type error"

inferUniverse :: Context -> Expr -> Either T.Text Int
inferUniverse ctx t = do
    u <- infer ctx t
    case nf ctx u of
      Universe k -> return k
      _ -> Left "type expected"

inferPi :: Context -> Expr -> Either T.Text Abstraction
inferPi ctx e = do
    t <- infer ctx e
    case nf ctx t of
      Pi a -> return a
      _ -> Left "function expected"

emptyContext :: Context
emptyContext = Map.empty

addType :: Name -> Expr -> Context -> Context
addType n t = Map.insert n (t, Nothing)

addDef :: Name -> Expr -> Expr -> Context -> Context
addDef n t e = Map.insert n (t, Just e)
