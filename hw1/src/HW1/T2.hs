module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus Z x = x
nplus x Z = x
nplus (S x) (S y) = nplus (S(S x)) y

nmult :: N -> N -> N
nmult Z _ = Z
nmult _ Z = Z
nmult (S x) (S y) = nplus (S x) (nmult (S x) (y))

nsub :: N -> N -> Maybe N
nsub x Z = Just x
nsub Z _ = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S x) (S y) = ncmp x y


nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S (nFromNatural (x - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S x) =  1 + nToNum(x)


nEven :: N -> Bool
nEven x = ncmp (nmod x (S(S(Z)))) (Z) == EQ

nOdd :: N -> Bool
nOdd n = not (nEven n)

ndiv :: N -> N -> N
ndiv _ Z = error "Division by zero."
ndiv (Z) (S _) = Z
ndiv x y = case (nsub x y) of
  Nothing -> Z
  _ -> S ((ndiv) (nFromJust (nsub (x) (y))) (y))
  
nmod :: N -> N -> N 
nmod _ Z = error "Division by zero."
nmod (Z) (S _) = Z
nmod x y = case (nsub x y) of
  Nothing -> x
  _ ->(nmod) (nFromJust (nsub (x) (y))) (y)
  

nFromJust :: Maybe N -> N
nFromJust (Just x) = x
nFromJust Nothing = Z
