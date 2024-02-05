{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , pEof
  , pAbbr
  ) where

import Numeric.Natural (Natural)
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Scientific (scientific, toRealFloat)
import HW4.Types
import HW4.T1

data ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P p) input = 
  case runES p (0, input) of
    Error e          -> Error e
    Success (a :# _) -> Success a 

pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

pCharExp :: Char -> Parser Char
pCharExp c = mfilter (==c) pChar

skipWhiteSpace :: Parser String
skipWhiteSpace = many $ pCharExp ' '

pDigits :: Parser String
pDigits = some $ mfilter isDigit pChar

parseError :: Parser a
parseError = P $ throwExceptState (ErrorAtPos 0)

instance Alternative Parser where
  empty = parseError
  (<|>) (P first) (P second) = P $ ES $ \(pos, s) ->
    case runES first (pos, s) of
      (Error _) -> runES second (pos, s)
      res       -> res 

instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) ->
  case s of
    []  -> Success(() :# (pos, s))
    _   -> Error (ErrorAtPos pos) 

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter isUpper pChar)
  pEof
  pure abbr

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (parseExpression <* pEof)

parseExpression :: Parser Expr
parseExpression = do 
  left <- parseTerm
  res  <- parseExpression' left
  pure res

parseExpression' :: Expr -> Parser Expr
parseExpression' left =  (parseAddSub left '+' Add) <|> (parseAddSub left '-' Sub) <|> pure left

parseAddSub:: Expr -> Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
parseAddSub left ch opConstruct = do   
  right <- pCharExp ch *> parseTerm
  res   <- parseExpression' $ Op(opConstruct left right)
  pure res

parseTerm :: Parser Expr
parseTerm = do 
  term <- parseFactor
  res  <- parseTerm' term
  pure res

parseTerm' :: Expr -> Parser Expr
parseTerm' left = (parseMulDiv left '*' Mul) <|> (parseMulDiv left '/' Div) <|> pure left

parseMulDiv:: Expr -> Char -> (Expr -> Expr -> Prim Expr) -> Parser Expr
parseMulDiv left ch opConstruct = do   
  right <- pCharExp ch *> parseFactor
  res   <- parseTerm' $ Op(opConstruct left right)
  pure res

parseFactor :: Parser Expr
parseFactor = skipWhiteSpace *> (parseBrackets <|> parseDouble) <* skipWhiteSpace

parseBrackets :: Parser Expr
parseBrackets = pCharExp '(' *> skipWhiteSpace *> parseExpression <* skipWhiteSpace <* pCharExp ')'

parseDouble :: Parser Expr
parseDouble = do
  before <- skipWhiteSpace *> pDigits
  after  <- (pCharExp '.' *> pDigits) <|> pure [] <* skipWhiteSpace
  double <- pure $ Val $ castDouble before after
  pure double

castDouble :: String -> String -> Double
castDouble before after = toRealFloat $ scientific (intDigits strDigits) degree
  where 
    strDigits = before <> after
    degree    = negate $ length after

splitDigits :: Int -> Char -> Int
splitDigits res c = res * 10 + digitToInt c

intDigits :: String -> Integer
intDigits strDigits = toInteger $ foldl splitDigits 0 strDigits
