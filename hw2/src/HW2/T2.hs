module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty 

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep a = foldr f b a
  where
  b = [] :| []
  f c (h :| t)
      | c == sep = [] :| (h : t) 
      | otherwise = (c : h) :| t 


joinWith :: a -> NonEmpty [a] -> [a]
joinWith sep a = foldr1 f a
  where
    f arr res = arr ++ [sep] ++ res
