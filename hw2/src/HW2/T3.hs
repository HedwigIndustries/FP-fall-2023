module HW2.T3
  ( epart
  , mcat
  ) where

mcat :: Monoid a => [Maybe a] -> a
mcat arr = foldMap f arr
  where
    f Nothing = mempty
    f (Just a) = a

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart arr = foldMap f arr
  where
    f (Left a) = (a, mempty) 
    f (Right b) = (mempty, b)
