{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Empty where

import Data.LnFunctor
import Type.Families (Rfl)

class (LnFunctor f, LnInitial f) => LnEmpty f where
  lempty :: Init f i j => f i j a

iempty :: (LnEmpty f, Rfl Init f i)
  => f i i a
iempty = lempty

