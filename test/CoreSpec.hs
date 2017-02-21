{-# LANGUAGE OverloadedStrings #-}
module CoreSpec where

import Test.Hspec
import Core
import Parser

alpha s1 s2 = case parseExpr s1 of
                Right e1 -> case parseExpr s2 of
                              Right e2 -> alphaEq e1 e2
beta s1 s2 = case parseExpr s1 of
               Right e1 -> case parseExpr s2 of
                             Right e2 -> betaEq emptyContext e1 e2

spec :: Spec
spec = do
    describe "alphaEq" $ do
        it "x == x" $
            alpha "x" "x" `shouldBe` True
        it "x != y" $
            alpha "x" "y" `shouldBe` False
        it "\\x : y . x == \\a : t . a" $
            alpha "fun x : y => x" "fun a : y => a" `shouldBe` True
        it "(\\x : y -> y . x)(\\x : y . x) == (\\a : y -> y . a)(\\b : y . b)" $
            alpha "(fun x : y -> y => x) (fun x : y => x)"
                  "(fun a : y -> y => a) (fun b : y => b)"
                  `shouldBe` True
        it "\\a : Type 0 . a == \\b : Type 0 . b" $
            alpha "fun a : Type 0 => a"
                  "fun b : Type 0 => b"
                  `shouldBe` True
    describe "betaEq" $ do
        it "(\\x : Type 0 . \\x : x . x) y == \\x : y . x" $
            beta "(fun x : Type 0 => fun x : x => x) y"
                 "fun x : y => x"
                 `shouldBe` True
        it "(\\A : Type 0 . \\x : A . x) N z == z" $
            beta "(fun A : Type 0 => fun x : A => x) N z"
                 "z"
                 `shouldBe` True
