{-# LANGUAGE OverloadedStrings #-}
module Core where

import qualified Data.Text as T
import Syntax
import Data.List (union, (\\))

freeVars :: Expr -> [Name]
freeVars (Var name) = [name]
freeVars (Universe _) = []
freeVars (Pi abstr) = freeVars (typ abstr) `union` (freeVars (body abstr) \\ [bound abstr])
freeVars (Lambda abstr) = freeVars (typ abstr) `union` (freeVars (body abstr) \\ [bound abstr])
freeVars (App f a) = freeVars f `union` freeVars a
