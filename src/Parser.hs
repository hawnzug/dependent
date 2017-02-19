{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Functor.Identity (Identity)
import Text.Parsec
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import qualified Text.Parsec.Language as Lang

import Syntax

lexer :: Tok.GenTokenParser T.Text () Identity
lexer = Tok.makeTokenParser style

style :: Tok.GenLanguageDef T.Text () Identity
style = Lang.emptyDef
    { Tok.commentStart    = "{-"
    , Tok.commentEnd      = "-}"
    , Tok.commentLine     = "--"
    , Tok.nestedComments  = True
    , Tok.identStart      = letter
    , Tok.identLetter     = alphaNum <|> oneOf "_'"
    , Tok.opStart         = Tok.opLetter style
    , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Tok.reservedOpNames = []
    , Tok.reservedNames   = []
    , Tok.caseSensitive   = True
    }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

reservedOp :: T.Text -> Parser ()
reservedOp op = Tok.reservedOp lexer (T.unpack op)

ident :: Parser T.Text
ident = T.pack <$> Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

var :: Parser Expr
var = do
    var <- ident
    return (Var var )

app :: Parser Expr
app = do
    e1 <- expr
    e2 <- expr
    return (App e1 e2)

universe :: Parser Expr
universe = do
    reservedOp "Type"
    num <- natural
    return (Universe (fromIntegral num))

abstr :: T.Text -> Parser Abstraction
abstr arrow = do
    binder <- ident
    reservedOp ":"
    typ <- expr
    reservedOp arrow
    rhs <- expr
    return Abstraction {bound = binder, typ = typ, body = rhs}

fun :: Parser Expr
fun = do
    reservedOp "fun"
    abs <- abstr "=>"
    return (Lambda abs)

tpi :: Parser Expr
tpi = do
    reservedOp "forall"
    abs <- abstr "->"
    return (Pi abs)

expr :: Parser Expr
expr = do
    es <- many1 aexp
    return (foldl1 App es)

aexp :: Parser Expr
aexp = tpi <|> universe <|> fun <|> var <|> parens expr

parseExpr :: T.Text -> Expr
parseExpr input =
    case parse expr "<stdin>" input of
      Left err -> error (show err)
      Right ast -> ast
