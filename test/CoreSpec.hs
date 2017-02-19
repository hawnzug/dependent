{-# LANGUAGE OverloadedStrings #-}
module CoreSpec where

import Test.Hspec
import Core
import Parser

spec :: Spec
spec = describe "freeVars" $ do
    it "something" $
        freeVars (parseExpr "x") `shouldBe` ["x"]
    it "something" $
        freeVars (parseExpr "fun x : N => x") `shouldBe` ["N"]
