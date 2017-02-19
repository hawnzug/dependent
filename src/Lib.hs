{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( repl
    ) where

import System.Console.Haskeline
import qualified Data.Text as T

import Core
import Parser
import Pretty

repl :: IO ()
repl = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ T.unpack $ pretty (nf emptyContext (parseExpr (T.pack input)))
                                loop
