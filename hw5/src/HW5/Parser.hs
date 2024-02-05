module HW5.Parser
  ( parse
  ) where

import HW5.Base
import Data.Void (Void)
import Data.Text as T (pack)
-- import Data.ByteString as B (pack)
import Control.Applicative (many, (<|>), empty)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Text.Megaparsec.Char 
  (
    string,
    char,
    space1
  )
import qualified Text.Megaparsec.Char.Lexer as L 
  (
    space,
    lexeme,
    symbol,
    signed,
    scientific,
    charLiteral,
    hexadecimal
  )
import Text.Megaparsec
  ( 
    ParseErrorBundle,
    Parsec,
    between,
    choice,
    eof,
    manyTill,
    runParser,
    sepBy,
    notFollowedBy,
    try
 )
import Data.ByteString as B (pack)

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (between pSpace eof pExpr) ""

pSpace :: Parser ()
pSpace = L.space space1 empty empty 

pLex :: Parser a -> Parser a
pLex = L.lexeme pSpace

pSymbol :: String -> Parser String
pSymbol = L.symbol pSpace

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

pBinaryL :: String -> HiFun -> Operator Parser HiExpr
pBinaryL "/" f = InfixL $ (wHiFun f) <$ try (pLex (char '/' <* notFollowedBy (char '=')))
pBinaryL name f = InfixL $ (wHiFun f) <$ pSymbol name

pBinaryN :: String -> HiFun -> Operator Parser HiExpr
pBinaryN name f = InfixN $ (wHiFun f) <$ pSymbol name

pBinaryR :: String -> HiFun -> Operator Parser HiExpr
pBinaryR name f = InfixR $ (wHiFun f) <$ pSymbol name

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ 
    [ 
      pBinaryL "*" HiFunMul,
      pBinaryL "/" HiFunDiv
    ]
  ,
    [ 
      pBinaryL "+" HiFunAdd,
      pBinaryL "-" HiFunSub
    ]
  ,
    [
      pBinaryN "==" HiFunEquals,
      pBinaryN "/=" HiFunNotEquals,
      pBinaryN ">=" HiFunNotLessThan,
      pBinaryN "<=" HiFunNotGreaterThan,
      pBinaryN "<" HiFunLessThan,
      pBinaryN ">" HiFunGreaterThan
    ]
  ,
    [
      pBinaryR "&&" HiFunAnd 
    ]
  ,
    [ 
      pBinaryR "||"  HiFunOr 
    ]
  ]


wHiFun:: HiFun -> HiExpr -> HiExpr -> HiExpr
wHiFun f x y = HiExprApply ((HiExprValue . HiValueFunction) f) [x, y]

pTerm :: Parser HiExpr
pTerm = choice
  [ 
    parens pExpr,
    pHiValue
  ]

pHiValue :: Parser HiExpr
pHiValue = do 
  essense <- pEssense <|> pList
  args    <- many $ parens pArgs
  return $ foldl HiExprApply essense args

pEssense:: Parser HiExpr
pEssense = 
  pLex $ 
    HiExprValue <$> choice
      [
        pNumber,
        pHiFun,
        pBool, 
        pNull,
        pString,
        pBytes
      ] 

pNumber :: Parser HiValue
pNumber = (.) HiValueNumber toRational <$> L.signed pSpace L.scientific

pBool :: Parser HiValue
pBool = (<$>) HiValueBool $ True <$ string "true" <|> False <$ string "false" 

pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

pString :: Parser HiValue
pString = (<$>) HiValueString T.pack <$> (char '\"' *> manyTill L.charLiteral (char '\"'))

pList :: Parser HiExpr
pList = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$> (brackets pArgs) 

pBytes :: Parser HiValue
pBytes = HiValueBytes . B.pack <$> bracers (many (pLex L.hexadecimal))

pHiFun :: Parser HiValue
pHiFun = 
  pLex $ 
    HiValueFunction <$> choice
        [
          HiFunAdd <$ string "add",
          HiFunSub <$ string "sub",
          HiFunMul <$ string "mul",
          HiFunDiv <$ string "div",
          HiFunAnd <$ string "and",
          HiFunOr <$ string "or",
          HiFunLessThan <$ string "less-than",
          HiFunGreaterThan <$ string "greater-than",
          HiFunEquals <$ string "equals",
          HiFunNotLessThan <$ string "not-less-than",
          HiFunNotGreaterThan <$ string "not-greater-than",
          HiFunNotEquals <$ string "not-equals",
          HiFunNot <$ string "not",
          HiFunIf <$ string "if",
          HiFunLength <$ string "length",
          HiFunToUpper <$ string "to-upper",
          HiFunToLower <$ string "to-lower",
          HiFunReverse <$ string "reverse",
          HiFunTrim <$ string "trim",
          HiFunList <$ string "list",
          HiFunRange <$ string "range",
          HiFunFold <$ string "fold",
          HiFunPackBytes <$ string "pack-bytes",
          HiFunUnpackBytes <$ string "unpack-bytes",
          HiFunZip <$ string "zip",
          HiFunUnzip <$ string "unzip",
          HiFunEncodeUtf8 <$ string "encode-utf8",
          HiFunDecodeUtf8 <$ string "decode-utf8",
          HiFunSerialise <$ string "serialise",
          HiFunDeserialise <$ string "deserialise"
        ]

pArgs:: Parser [HiExpr]
pArgs = pLex $ pExpr `sepBy` pSymbol ","

parens :: Parser a -> Parser a
parens = between (pSymbol "(") (pSymbol ")")

brackets :: Parser a -> Parser a
brackets = between (pSymbol "[") (pSymbol "]")

bracers :: Parser a -> Parser a
bracers = between (pSymbol "[#") (pSymbol "#]")