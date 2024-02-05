module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import HW4.Types

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f es = ES $ \s -> 
  case runES es s of 
    Error e           -> Error e 
    Success (a :# s') -> Success ((f a) :# s')

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState es = ES $ \s ->
  case runES es s of
    Error e             -> Error e
    Success (es' :# s') -> runES es' s'

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success(() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  es1 <*> es2 = do
    s1 <- es1
    s2 <- es2
    return (s1 s2) 

instance Monad (ExceptState e s) where
  return   = wrapExceptState
  es >>= f = joinExceptState (fmap f es)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x)       = return x
eval (Op(Abs x))   = invokeUnOP x Abs abs 
eval (Op(Sgn x))   = invokeUnOP  x Sgn signum
eval (Op(Add x y)) = invokeBinOP x y Add (+) '+'
eval (Op(Sub x y)) = invokeBinOP x y Sub (-) '-'
eval (Op(Mul x y)) = invokeBinOP x y Mul (*) '*'
eval (Op(Div x y)) = invokeBinOP x y Div (/) '/'

invokeUnOP :: Expr -> (Double -> Prim Double) -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
invokeUnOP x opConstruct op = do
  x' <- eval x
  modifyExceptState (opConstruct x' :)
  return (op x')
  
invokeBinOP :: Expr -> Expr -> (Double -> Double -> Prim Double) -> (Double -> Double -> Double) -> Char -> ExceptState EvaluationError [Prim Double] Double
invokeBinOP x y opConstruct op c = do
  x' <- eval x
  y' <- eval y
  modifyExceptState (opConstruct x' y' :) 
  if y' == 0 && c == '/'
    then throwExceptState DivideByZero
    else return (op x' y')
