module HW5.Base
  ( HiError (..)
  , HiExpr (..) 
  , HiFun (..)
  , HiValue (..)
  ) where

import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)

data HiFun  = 
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  deriving (Show, Eq, Ord)

data HiValue =
  HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  deriving (Show, Eq, Ord)

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  deriving (Show)

data HiError = 
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)
