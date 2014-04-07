{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Pointed where

import Data.LnFunctor
import Type.Families (Rfl)

class (LnFunctor f, LnInitial f) => LnPointed (f :: ix -> ix -> * -> *) where
  lreturn :: Init f i j => a -> f i j a

ireturn :: (LnPointed f, Rfl Init f i) => a -> f i i a
ireturn = lreturn

