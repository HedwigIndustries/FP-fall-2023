module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Numeric.Natural (Natural)
import Data.Function(fix)

repeat' :: a -> [a]
repeat' x = fix (x:) 

map' :: (a -> b) -> [a] -> [b]
map' f = fix $
  \rec x -> 
    case x of 
      []     -> []
      (y:ys) -> f y : rec ys    
  
fib :: Natural -> Natural
fib n = fst $ fix 
  (\rec n' (x,y) -> 
      case n' of
        0 -> (x,y)
        _ -> rec (n'-1) (y, x + y) 
    ) n (0,1) 


fac :: Natural -> Natural
fac = fix $
  \rec n -> 
    case n of
      0 -> 1
      1 -> 1 
      _ -> n * rec(n-1)
