{-# LANGUAGE OverloadedStrings #-}

module HW5.Evaluator
  ( eval
  ) where

import HW5.Base
import Control.Monad.Except 
  (
    ExceptT (ExceptT),
    runExceptT,
    foldM
  )
import GHC.Real (Ratio ((:%)), numerator, denominator)
import Data.Semigroup (stimes)
import Codec.Serialise (deserialise, serialise)
import qualified Data.Text as T 
  (
    Text,
    append,
    concat,
    length,
    toLower,
    toUpper,
    reverse,
    singleton,
    strip,
    index,
    take,
    drop
  )
import Codec.Compression.Zlib
  ( bestCompression,
    compressLevel,
    compressWith,
    decompress,
    defaultCompressParams,
  )
import qualified Data.Sequence as S
  ( 
    Seq,
    Seq ((:<|)),
    length,
    reverse,
    index,
    take,
    drop,
    fromList,
    fromFunction,
    (><),
  )
import qualified Data.ByteString as B
  ( ByteString,
    append,
    drop,
    index,
    length,
    pack,
    reverse,
    take,
    unpack,
  )
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Foldable (toList)

type HiExcept m = ExceptT HiError m HiValue

eval :: Monad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalExpr

evalExpr :: Monad m => HiExpr -> HiExcept m
evalExpr (HiExprValue val)      = return val
evalExpr (HiExprApply fun args) = do
  fun' <- evalExpr fun
  apply fun' args 

binary :: Monad m => ((HiValue, HiValue) -> HiExcept m) -> [HiExpr] -> HiExcept m
binary fun [x, y] = do
  x' <- evalExpr x
  y' <- evalExpr y
  fun (x', y')
binary _ _ = arityMismatch

unary :: Monad m => (HiValue -> HiExcept m) -> [HiExpr] -> HiExcept m
unary fun [x] = do
  x' <- evalExpr x
  fun x'
unary _ _ = arityMismatch

apply :: Monad m => HiValue -> [HiExpr] -> HiExcept m
apply (HiValueFunction HiFunAdd) args            = binary addDepends args
apply (HiValueFunction HiFunSub) args            = binary subDepends args
apply (HiValueFunction HiFunMul) args            = binary mulDepends args 
apply (HiValueFunction HiFunDiv) args            = binary divDepends args
apply (HiValueFunction HiFunNot) args            = unary logicNot args 
apply (HiValueFunction HiFunAnd) args            = logicAnd args 
apply (HiValueFunction HiFunOr) args             = logicOr  args 
apply (HiValueFunction HiFunLessThan) args       = binary (cmp (<)) args  
apply (HiValueFunction HiFunGreaterThan) args    = binary (cmp (>)) args
apply (HiValueFunction HiFunEquals) args         = binary (cmp (==)) args 
apply (HiValueFunction HiFunNotLessThan) args    = binary (cmp (>=)) args 
apply (HiValueFunction HiFunNotGreaterThan) args = binary (cmp (<=)) args
apply (HiValueFunction HiFunNotEquals) args      = binary (cmp (/=)) args 
apply (HiValueFunction HiFunIf) args             = ifState args 
apply (HiValueFunction HiFunLength) args         = unary lenDepends args 
apply (HiValueFunction HiFunToUpper) args        = unary toUpper args 
apply (HiValueFunction HiFunToLower) args        = unary toLower args
apply (HiValueFunction HiFunReverse) args        = unary reverseDepends args 
apply (HiValueFunction HiFunTrim) args           = unary trim args 
apply (HiValueString t) [idx]                    = unary (getIdxT t) [idx]
apply (HiValueString t) args                     = binary (getSliceT t) args
apply (HiValueFunction HiFunList) args           = buildList args
apply (HiValueList s) [idx]                      = unary (getIdxS s) [idx]
apply (HiValueList s) args                       = binary (getSliceS s) args
apply (HiValueFunction HiFunRange) args          = binary range args 
apply (HiValueFunction HiFunFold) args           = binary fold args 


apply (HiValueFunction HiFunPackBytes) args      = unary (packBytes) args
apply (HiValueFunction HiFunUnpackBytes) args    = unary (unpackBytes) args
apply (HiValueFunction HiFunZip) args            = unary (zipBytes) args
apply (HiValueFunction HiFunUnzip) args          = unary (unzipBytes) args
apply (HiValueFunction HiFunEncodeUtf8) args     = unary (enUTF8Bytes) args
apply (HiValueFunction HiFunDecodeUtf8) args     = unary (deUTF8Bytes) args
apply (HiValueFunction HiFunSerialise) args      = unary (serializeBytes) args
apply (HiValueFunction HiFunDeserialise) args    = unary (deserializeBytes) args

apply (HiValueBytes s) [x]                       = unary (getIdxB s) [x]
apply (HiValueBytes s) args                      = binary (getSliceB s) args
apply _ _                                        = invalidFunction

addDepends :: Monad m => (HiValue, HiValue) -> HiExcept m
addDepends (HiValueNumber x, HiValueNumber y) = wNumber $ x + y
addDepends (HiValueString x, HiValueString y) = wString $ T.append x y
addDepends (HiValueList x, HiValueList y)     = wList $ x S.>< y
addDepends (HiValueBytes x, HiValueBytes y)   = wBytes $ B.append x y
addDepends _                                  = invalidArgument

subDepends :: Monad m => (HiValue, HiValue) -> HiExcept m
subDepends (HiValueNumber x, HiValueNumber y) = wNumber $ x - y
subDepends _                                  = invalidArgument

mulDepends :: Monad m => (HiValue, HiValue) -> HiExcept m 
mulDepends (HiValueNumber x, HiValueNumber y)        = wNumber $ x * y
mulDepends (HiValueString x, HiValueNumber (y :% 1)) = wString $ stimes y x
mulDepends (HiValueList x, HiValueNumber (y :% 1))   = wList $ stimes y x
mulDepends (HiValueBytes x, HiValueNumber (y :% 1))  = wBytes $ stimes y x
mulDepends _                                         = invalidArgument

divDepends :: Monad m => (HiValue, HiValue) -> HiExcept m
divDepends (HiValueNumber _, HiValueNumber (0 :% _)) = divideByZero
divDepends (HiValueNumber x, HiValueNumber y)        = wNumber $ x / y
divDepends (HiValueString x, HiValueString y)        = wString $ T.concat [x, "/", y]
divDepends _                                         = invalidArgument

logicNot :: Monad m => HiValue -> HiExcept m 
logicNot (HiValueBool x) = wBool $ not x
logicNot _               = invalidArgument

logicAnd :: Monad m => [HiExpr] -> HiExcept m
logicAnd [x, y] = do
  x' <- evalExpr x
  case x' of 
    (HiValueBool False) -> return x'
    _                   -> evalExpr y
logicAnd _ = arityMismatch

logicOr :: Monad m => [HiExpr] -> HiExcept m
logicOr [x, y] = do
  x' <- evalExpr x
  case x' of 
    (HiValueBool False) -> evalExpr y
    _                   -> return x'
logicOr _ = arityMismatch

cmp :: Monad m => (HiValue -> HiValue -> Bool) -> (HiValue, HiValue) -> HiExcept m
cmp op (x, y) = wBool $ op x y 

ifState :: Monad m => [HiExpr] -> HiExcept m
ifState [x, y, z] = do
  x' <- evalExpr x 
  case x' of 
    (HiValueBool p) -> evalExpr $ if p then y else z
    _               -> invalidArgument 
ifState _ = arityMismatch

lenDepends :: Monad m => HiValue -> HiExcept m 
lenDepends (HiValueString x) = wNumber $ fromIntegral $ T.length x  
lenDepends (HiValueList x)   = wNumber $ fromIntegral $ S.length x  
lenDepends (HiValueBytes x)  = wNumber $ fromIntegral $ B.length x  
lenDepends _                 = invalidArgument

toUpper :: Monad m => HiValue -> HiExcept m 
toUpper (HiValueString x) = wString $ T.toUpper x 
toUpper _                 = invalidArgument

toLower :: Monad m => HiValue -> HiExcept m 
toLower (HiValueString x) = wString $ T.toLower x 
toLower _                 = invalidArgument

reverseDepends :: Monad m => HiValue -> HiExcept m 
reverseDepends (HiValueString x)  = wString $ T.reverse x 
reverseDepends (HiValueList x)    = wList $ S.reverse x 
reverseDepends (HiValueBytes x)   = wBytes $ B.reverse x 
reverseDepends _                  = invalidArgument

trim :: Monad m => HiValue -> HiExcept m 
trim (HiValueString x) = wString $ T.strip x 
trim _                 = invalidArgument

getIdxT :: Monad m => T.Text -> HiValue -> HiExcept m 
getIdxT s (HiValueNumber (idx :% 1))  = 
    if idx' >= 0 && idx' < T.length s 
      then wString $ T.singleton $ T.index s idx'
      else return HiValueNull
        where idx' = fromIntegral idx
getIdxT _ _ = invalidArgument

getSliceT :: Monad m => T.Text -> (HiValue, HiValue) -> HiExcept m 
getSliceT s (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) = 
  wString $ T.take (e' - s') (T.drop s' s)   
      where 
        s' = checkPtrT s $ fromIntegral start 
        e' = checkPtrT s $ fromIntegral end
getSliceT s (HiValueNumber (start :% 1), HiValueNull) = 
  wString $ T.take (T.length s - s') (T.drop s' s)   
      where 
        s' = checkPtrT s $ fromIntegral start 
getSliceT s (HiValueNull, HiValueNumber (end :% 1)) = 
  wString $ T.take (e') (T.drop 0 s)   
      where 
        e' = checkPtrT s $ fromIntegral end
getSliceT _ _ = invalidArgument

checkPtrT :: T.Text -> Int -> Int
checkPtrT s idx = 
  if idx >= 0
    then idx
    else idx + T.length s

getIdxS :: Monad m => S.Seq HiValue -> HiValue -> HiExcept m 
getIdxS s (HiValueNumber (idx :% 1))  = 
    if idx' >= 0 && idx' < S.length s 
      then return $ S.index s idx'
      else return HiValueNull
        where idx' = fromIntegral idx
getIdxS _ _ = invalidArgument

getSliceS :: Monad m => S.Seq HiValue -> (HiValue, HiValue) -> HiExcept m 
getSliceS s (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) = 
  wList $ S.take (e' - s') (S.drop s' s)   
      where 
        s' = checkPtrS s $ fromIntegral start 
        e' = checkPtrS s $ fromIntegral end
getSliceS s (HiValueNumber (start :% 1), HiValueNull) = 
  wList $ S.take (S.length s - s') (S.drop s' s)   
      where 
        s' = checkPtrS s $ fromIntegral start 
getSliceS s (HiValueNull, HiValueNumber (end :% 1)) = 
  wList $ S.take (e') (S.drop 0 s)   
      where 
        e' = checkPtrS s $ fromIntegral end
getSliceS _ _ = invalidArgument

checkPtrS :: S.Seq HiValue -> Int -> Int
checkPtrS s idx = 
  if idx >= 0
    then idx
    else idx + S.length s

buildList :: Monad m => [HiExpr] -> HiExcept m
buildList args = do
  args' <- mapM evalExpr args
  return $ HiValueList $ S.fromList args'

range :: Monad m => (HiValue, HiValue) -> HiExcept m
range (HiValueNumber start, HiValueNumber end) = 
  wList $ S.fromFunction count buildF
    where 
      count  = fromIntegral $ quot (numerator (end - start)) (denominator (end - start)) + 1
      buildF = (\x -> HiValueNumber $ fromIntegral x + start)
range _ = invalidArgument

fold :: Monad m => (HiValue, HiValue) -> HiExcept m
fold (f, HiValueList (h S.:<| t)) = foldM (\x y -> apply f [HiExprValue x, HiExprValue y]) h t 
fold _ = invalidArgument


packBytes :: Monad m => HiValue -> HiExcept m 
packBytes (HiValueList s) = 
  (HiValueBytes . B.pack . toList <$> mapM getInt s)
    where 
      getInt (HiValueNumber (x :% 1)) = return $ fromIntegral x  
packBytes _               = undefined

unpackBytes :: Monad m => HiValue -> HiExcept m
unpackBytes (HiValueBytes b) = wList $ S.fromList $ map ((.) HiValueNumber fromIntegral) (B.unpack b)  
unpackBytes _                = invalidArgument 

zipBytes :: Monad m => HiValue -> HiExcept m
zipBytes (HiValueBytes b) = wBytes $ toStrict $ compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict b
zipBytes _                = invalidArgument 


unzipBytes :: Monad m => HiValue -> HiExcept m
unzipBytes (HiValueBytes b) = wBytes $ toStrict $ decompress $ fromStrict b
unzipBytes _                = invalidArgument 

enUTF8Bytes :: Monad m => HiValue -> HiExcept m
enUTF8Bytes (HiValueString s) = wBytes $ encodeUtf8 s
enUTF8Bytes _                 = invalidArgument

deUTF8Bytes :: Monad m => HiValue -> HiExcept m
deUTF8Bytes (HiValueBytes b) = return $ either (const HiValueNull) HiValueString $ decodeUtf8' b
deUTF8Bytes _                = invalidArgument

serializeBytes :: Monad m => HiValue -> HiExcept m
serializeBytes args = (wBytes . toStrict . serialise) args
serializeBytes _ = undefined

deserializeBytes :: Monad m => HiValue -> HiExcept m
deserializeBytes (HiValueBytes b) = return $ deserialise $ fromStrict b
deserializeBytes _                = invalidArgument 

getIdxB :: Monad m => B.ByteString -> HiValue -> HiExcept m 
getIdxB s (HiValueNumber (idx :% 1))  = 
    if idx' >= 0 && idx' < B.length s 
      then wNumber $ fromIntegral (B.index s idx')
      else return HiValueNull
        where idx' = fromIntegral idx
getIdxB _ _ = invalidArgument

getSliceB :: Monad m => B.ByteString -> (HiValue, HiValue) -> HiExcept m 
getSliceB s (HiValueNumber (start :% 1), HiValueNumber (end :% 1)) = 
  wBytes $ B.take (e' - s') (B.drop s' s)   
      where 
        s' = checkPtrB s $ fromIntegral start 
        e' = checkPtrB s $ fromIntegral end
getSliceB s (HiValueNumber (start :% 1), HiValueNull) = 
  wBytes $ B.take (B.length s - s') (B.drop s' s)   
      where 
        s' = checkPtrB s $ fromIntegral start 
getSliceB s (HiValueNull, HiValueNumber (end :% 1)) = 
  wBytes $ B.take (e') (B.drop 0 s)   
      where 
        e' = checkPtrB s $ fromIntegral end
getSliceB _ _ = invalidArgument

checkPtrB :: B.ByteString -> Int -> Int
checkPtrB s idx = 
  if idx >= 0
    then idx
    else idx + B.length s

invalidArgument :: Monad m => HiExcept m
invalidArgument = ExceptT $ return $ Left HiErrorInvalidArgument

invalidFunction :: Monad m => HiExcept m
invalidFunction = ExceptT $ return $ Left HiErrorInvalidFunction

arityMismatch :: Monad m => HiExcept m
arityMismatch = ExceptT $ return $ Left HiErrorArityMismatch 

divideByZero :: Monad m => HiExcept m
divideByZero = ExceptT $ return $ Left HiErrorDivideByZero 

wNumber :: Monad m => Rational -> HiExcept m
wNumber = (.) return HiValueNumber

wBool :: Monad m => Bool -> HiExcept m 
wBool = (.) return HiValueBool 

wString :: Monad m => T.Text -> HiExcept m 
wString = (.) return HiValueString

wList :: Monad m => S.Seq HiValue -> HiExcept m 
wList = (.) return HiValueList

wBytes :: Monad m => B.ByteString -> HiExcept m 
wBytes = return . HiValueBytes
