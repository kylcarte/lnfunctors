{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnApplicative
  ( module Control.LnApplicative
  -- Convenience Reexports
  , module Data.LnFunctor.Apply
  , module Data.LnFunctor.Alt
  ) where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Alt

-- | This class defines an operation 'lpure', which can
-- put a value into the LnFunctor, with indices that are
-- constrained by the 'Init' associated type of the 'LnInitial' class.
--
-- Along with 'LnApply', this forms the linked analogue of the 'Applicative' class.
class (LnApply f, LnInitial f) => LnApplicative f where
  lpure :: Init f i j => a -> f i j a

-- | Type restricted form of 'lpure'
pure :: (LnApplicative f, Init f i i) => a -> f i i a
pure = lpure

-- | This class defines an empty value 'lempty', whose indices
-- are constrained by the 'Init' type.
class (LnApplicative f, LnAlt f) => LnAlternative f where
  lempty :: Init f i j => f i j a

-- | Type restricted form of 'lempty'
empty :: (LnAlternative f, Init f i i)
  => f i i a
empty = lempty

