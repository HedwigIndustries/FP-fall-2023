{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HW6.T2
  ( TSet

  , Contains
  , Add
  , Delete 
  ) where

import GHC.TypeLits

type TSet = [Symbol]

type family Contains (name :: Symbol) (set :: TSet) :: Bool where
  Contains name '[]         = 'False
  Contains name (name ': _) = 'True
  Contains name (_ ': tail) = Contains name tail

type family Delete (name :: Symbol) (set :: TSet) :: TSet where
  Delete name '[]             = '[]
  Delete name (name  ': tail) = tail
  Delete name (name' ': tail) = name' ': (Delete name tail)
  
type family Add (v :: Symbol) (set :: TSet) :: TSet where
  Add v '[]          = v  ': '[]
  Add v (v ': tail)  = v  ': tail
  Add v (v' ': tail) = v' ': (Add v tail)
