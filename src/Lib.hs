{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( repl
    ) where

import System.Console.Haskeline
import qualified Data.Text as T

import Core (nf, Context, emptyContext, addType, addDef, infer)
import Parser (parseExpr, parseCommand)
import Syntax
import Pretty

repl :: IO ()
repl = runInputT defaultSettings (loop emptyContext)
   where
       loop :: Context -> InputT IO ()
       loop ctx = do
           minput <- getInputLine "λ> "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> let (newCtx, output) = command ctx input in
                                 do outputStrLn output
                                    loop newCtx

command :: Context -> String -> (Context, String)
command ctx input =
    case parseCommand (T.pack input) of
      Left err -> (ctx, show err)
      Right (Parameter n t) -> let newCtx = addType n t ctx
                                   output = T.append n " is assumed."
                                in (newCtx, T.unpack output)
      Right (Definition n e) -> let newCtx = addDef n t e ctx
                                    t = infer ctx e
                                    output = T.append n " is assumed."
                                 in (newCtx, T.unpack output)
      Right (Eval e) -> (ctx, T.unpack $ pretty (nf ctx e))
      Right (Check e) -> (ctx, T.unpack $ pretty (infer ctx e))
