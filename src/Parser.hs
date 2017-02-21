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

reserved :: T.Text -> Parser ()
reserved name = Tok.reserved lexer (T.unpack name)

ident :: Parser T.Text
ident = T.pack <$> Tok.identifier lexer

natural :: Parser Integer
natural = Tok.natural lexer

var :: Parser Expr
var = do
    var <- ident
    return (Var var )

universe :: Parser Expr
universe = do
    reserved "Type"
    num <- natural
    return (Universe (fromIntegral num))

abstr :: T.Text -> Parser Abstraction
abstr arrow = do
    binder <- ident
    reservedOp ":"
    typ <- expr
    reservedOp arrow
    rhs <- expr
    return Abstraction{bound=binder, typ=typ, body=rhs}

fun :: Parser Expr
fun = do
    reservedOp "fun"
    abs <- abstr "=>"
    return (Lambda abs)

typeLeft :: Parser (Name, Expr)
typeLeft = do
    binder <- try $ do
        reservedOp "("
        binder <- ident
        reservedOp ":"
        return binder
    typ <- expr
    reservedOp ")"
    return (binder, typ)

expr :: Parser Expr
expr = scan
    where scan = do ee <- expr1
                    case ee of
                      Left e -> rest "_" e <|> return e
                      Right (n, e) -> rest n e
          rest n t = do reservedOp "->"
                        e <- scan
                        return (Pi Abstraction{bound=n, typ=t, body=e})


expr1 :: Parser (Either Expr (Name, Expr))
expr1 = fmap Right typeLeft <|>
        do es <- many1 aexp
           return (Left (foldl1 App es))

aexp :: Parser Expr
aexp = universe <|> fun <|> var <|> parens expr

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

parseExpr :: T.Text -> Either ParseError Expr
parseExpr = parse (contents expr) "<stdin>"

para :: Parser Command
para = do
    reserved "Parameter"
    n <- ident
    reservedOp ":"
    t <- expr
    return (Parameter n t)

def :: Parser Command
def = do
    reserved "Definition"
    n <- ident
    reservedOp ":="
    e <- expr
    return (Definition n e)

check :: Parser Command
check = do
    reserved "Check"
    e <- expr
    return (Check e)

eval :: Parser Command
eval = do
    reserved "Eval"
    e <- expr
    return (Eval e)

command :: Parser Command
command = para <|> def <|> check <|> eval

parseCommand :: T.Text -> Either ParseError Command
parseCommand = parse (contents command) "<stdin>"
