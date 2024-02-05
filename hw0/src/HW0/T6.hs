module HW0.T6
  ( a
  , a_whnf
  , b
  , b_whnf
  , c
  , c_whnf
  ) where

import HW0.T1 (distrib)
import Data.Char (isSpace)

a = distrib (Left ("AB" ++ "CD" ++ "EF"))

a_whnf = (Left "ABCDEF", Left "ABCDEF")

b = map isSpace "Hello, World"

b_whnf = [False,False,False,False,False,False,True,False,False,False,False,False]

c = if 1 > 0 || error "X" then "Y" else "Z"

c_whnf = "Y"