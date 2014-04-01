{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Alt where

import Data.LnFunctor
import Data.IxFunctor.Alt

type LnAlt m = WithLink IxAlt m
type AltLinks m ijs = Links IxAlt m ijs

{-
class IxFunctor m => IxAlt m where
  ialt  :: m i j a -> m i j a -> m i j a
  isome :: m i j a -> m i j [a]
  imany :: m i j a -> m i j [a]

(<||>) :: IxAlt m => m i j a -> m i j a -> m i j a
(<||>) = ialt
-}

