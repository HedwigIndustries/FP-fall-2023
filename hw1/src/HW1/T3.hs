module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch meta _ _ _) = meta

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch _ left _ right) = 1 + (max (tdepth(left)) (tdepth(right)))

tmember :: Ord a => a -> Tree a -> Bool
tmember value Leaf = False
tmember value (Branch _ left node right) 
    |(value == node) = True
    |(value > node) = tmember value right
    |(value < node) = tmember value left

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert value Leaf = (Branch 1 Leaf value Leaf)
tinsert value (Branch meta left node right) 
    |(value > node) = (Branch (meta + 1) left node (tinsert value right))
    |(value < node) = (Branch (meta + 1) (tinsert value left) node right)
    |otherwise = (Branch meta left node right) 

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList (value:tail) = tinsert value (tFromList tail)
