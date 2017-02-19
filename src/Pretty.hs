{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Syntax
import qualified Data.Text as T

pretty :: Expr -> T.Text
pretty (Var x) = x
pretty (Universe k) = T.pack (show k)
pretty (App f a) = T.concat ["(", pretty f, pretty a, ")"]
pretty (Lambda abstr) = prettyAbs "fun " " => " abstr
pretty (Pi abstr) = prettyAbs "forall " " -> " abstr

prettyAbs :: T.Text -> T.Text -> Abstraction -> T.Text
prettyAbs x y abstr = T.concat [x, n, " : ", pretty t, y, pretty b]
    where n = bound abstr
          t = typ abstr
          b = body abstr
