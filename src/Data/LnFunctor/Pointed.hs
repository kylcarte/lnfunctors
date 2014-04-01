{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Pointed where

import Data.LnFunctor
import Data.IxFunctor.Pointed (IxPointed)
import qualified Data.IxFunctor.Pointed as I

type LnPointed m = WithLink IxPointed m
type PointedLinks m ijs = Links IxPointed m ijs

{-

class IxFunctor m => IxPointed m where
  ireturn :: a -> m i i a

-}

