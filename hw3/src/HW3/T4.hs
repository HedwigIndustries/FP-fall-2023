module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState = undefined

wrapState :: a -> State s a
wrapState = undefined

joinState :: State s (State s a) -> State s a
joinState = undefined

modifyState :: (s -> s) -> State s ()
modifyState = undefined

instance Functor (State s) where
  fmap = undefined

instance Applicative (State s) where
  pure = undefined
  (<*>) = undefined

instance Monad (State s) where
  (>>=) = undefined

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) = undefined
  (-) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined

instance Fractional Expr where
  (/) = undefined
  fromRational = undefined

eval :: Expr -> State [Prim Double] Double
eval = undefined
