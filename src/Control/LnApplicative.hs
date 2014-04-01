{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnApplicative
  ( module Control.LnApplicative
  -- Convenience Reexports
  , module Data.LnFunctor.Pointed
  , module Data.LnFunctor.Apply
  , module Data.LnFunctor.Empty
  , module Data.LnFunctor.Alt
  ) where

import Data.LnFunctor
-- Applicative
import Data.LnFunctor.Pointed
import Data.LnFunctor.Apply
-- Alternative
import Data.LnFunctor.Empty
import Data.LnFunctor.Alt

type LnApplicative m = (LnPointed m, LnApply m)

type LnAlternative m = (LnApplicative m, LnEmpty m, LnAlt m)

