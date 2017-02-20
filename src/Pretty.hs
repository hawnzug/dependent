{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Syntax
import qualified Data.Text as T
import Core (freeVars)

pretty :: Expr -> T.Text
pretty (Var x) = x
pretty (Universe k) = "Type" `T.append` T.pack (show k)
pretty (App f a) = T.concat ["(", pretty f, " ", pretty a, ")"]
pretty (Lambda Abstraction{bound=n, typ=t, body=b}) = T.concat ["(fun ", n, " : ", pretty t, " => ", pretty b, ")"]
pretty (Pi Abstraction{bound=n, typ=t, body=b}) = if n `elem` freeVars b
                                                     then T.concat ["(forall ", n, " : ", pretty t, " -> ", pretty b, ")"]
                                                     else T.concat ["(", pretty t, " -> ", pretty b, ")"]
