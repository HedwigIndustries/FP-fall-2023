module HW5.Pretty
  ( prettyValue
  ) where

import HW5.Base
import GHC.Real (Ratio ((:%)))
import Data.Foldable (toList)
import Data.Scientific 
  (
    Scientific,
    floatingOrInteger,
    fromRationalRepetendUnlimited
  )
import Data.Sequence (Seq)
import Data.Text (Text)
import Prettyprinter.Render.Terminal (AnsiStyle)
import Prettyprinter
  (
    Doc,
    pretty,
    (<+>),
    slash,
    dquotes,
    list,
    space,
    encloseSep
  )
import GHC.Word (Word8)
import Data.ByteString as B (ByteString, unpack)
import Numeric (showHex)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction f) = prettyFunction f
prettyValue (HiValueNumber num) = prettyNumber num
prettyValue (HiValueBool b)     = prettyBool b
prettyValue HiValueNull         = prettyNull
prettyValue (HiValueString s)   = prettyString s
prettyValue (HiValueList s)     = prettyList' s
prettyValue (HiValueBytes s)    = prettyBytes s


prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber num = 
  case fromRationalRepetendUnlimited num of
    (s, Nothing) -> prettyFloatOrInteger s
    _            -> prettyRational num

prettyFloatOrInteger :: Scientific -> Doc AnsiStyle
prettyFloatOrInteger s = 
  case floatingOrInteger s :: Either Double Integer of
    Left d  -> pretty d
    Right i -> pretty i

prettyRational :: Rational -> Doc AnsiStyle
prettyRational (x :% y) = 
  case quotRem x y of 
      (0, r) -> if sign == "-" then pretty sign <> correctF r else correctF r
      (q, r) -> pretty q <+> pretty sign <+> correctF r
  where 
    sign = if x > 0 then "+" else "-"
    correctF r = pretty (abs r) <> slash <> pretty y

prettyBool :: Bool -> Doc AnsiStyle
prettyBool True = pretty "true"
prettyBool False = pretty "false"

prettyNull :: Doc AnsiStyle
prettyNull = pretty "null"

prettyString :: Text -> Doc AnsiStyle
prettyString t = dquotes $ pretty t

prettyList' :: Seq HiValue -> Doc AnsiStyle
prettyList' s = list . map prettyValue . toList $ s

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes b = encloseSep (pretty "[# ") (pretty " #]") space $ (<$>) (prettyUnpack) $ B.unpack b

prettyUnpack :: Word8 -> Doc AnsiStyle
prettyUnpack = (pretty . twoDigitsCheck. flip showHex "")

twoDigitsCheck :: String -> String
twoDigitsCheck [s] = ['0', s]
twoDigitsCheck s = s

prettyFunction :: HiFun -> Doc AnsiStyle
prettyFunction HiFunAdd            = pretty "add"
prettyFunction HiFunSub            = pretty "sub"
prettyFunction HiFunMul            = pretty "mul"
prettyFunction HiFunDiv            = pretty "div"
prettyFunction HiFunNot            = pretty "not"
prettyFunction HiFunAnd            = pretty "and"
prettyFunction HiFunOr             = pretty "or"
prettyFunction HiFunLessThan       = pretty "less-than"
prettyFunction HiFunGreaterThan    = pretty "greater-than"
prettyFunction HiFunEquals         = pretty "equals"
prettyFunction HiFunNotLessThan    = pretty "not-less-than"
prettyFunction HiFunNotGreaterThan = pretty "not-greater-than"
prettyFunction HiFunNotEquals      = pretty "not-equals"
prettyFunction HiFunIf             = pretty "if"
prettyFunction HiFunLength         = pretty "lenght"
prettyFunction HiFunToUpper        = pretty "to-upper"
prettyFunction HiFunToLower        = pretty "to-lower"
prettyFunction HiFunReverse        = pretty "reverse"
prettyFunction HiFunTrim           = pretty "trim"
prettyFunction HiFunFold           = pretty "fold"
prettyFunction HiFunList           = pretty "list"
prettyFunction HiFunRange          = pretty "range"
prettyFunction HiFunPackBytes      = pretty "pack-bytes"
prettyFunction HiFunUnpackBytes    = pretty "unpack-bytes"
prettyFunction HiFunZip            = pretty "zip"
prettyFunction HiFunUnzip          = pretty "unzip"
prettyFunction HiFunSerialise      = pretty "serialise"
prettyFunction HiFunDeserialise    = pretty "deserialise"
prettyFunction HiFunEncodeUtf8     = pretty "encode-utf8"
prettyFunction HiFunDecodeUtf8     = pretty "decode-utf8"