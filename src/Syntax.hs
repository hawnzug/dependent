{-# LANGUAGE OverloadedStrings #-}
module Syntax where

import qualified Data.Text as T

data Expr = Var Name
          | Universe Int
          | Pi Abstraction
          | Lambda Abstraction
          | App Expr Expr
          deriving (Show, Eq)

data Abstraction = Abstraction { bound :: Name
                               , typ :: Expr
                               , body :: Expr}
                               deriving (Show, Eq)

type Name = T.Text
