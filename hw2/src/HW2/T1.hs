module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf = b
tfoldr f b (Branch _ l n r) = tfoldr f (f n (tfoldr f b r)) l   
