{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Empty where

import Data.LnFunctor

-- | This class defines an empty functoral value
class (LnFunctor f, LnInitial f) => LnEmpty f where
  lempty :: Init f i j => f i j a

iempty :: (LnEmpty f, Init f i i)
  => f i i a
iempty = lempty

