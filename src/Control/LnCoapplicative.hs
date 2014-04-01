{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnCoapplicative
  ( module Control.LnCoapplicative
  -- Convenience Reexports
  , module Data.LnFunctor.Copointed
  , module Data.LnFunctor.Coapply
  , module Data.LnFunctor.Empty
  , module Data.LnFunctor.Coalt
  ) where

import Data.LnFunctor
-- Coapplicative
import Data.LnFunctor.Copointed
import Data.LnFunctor.Coapply
-- Coalternative
import Data.LnFunctor.Empty
import Data.LnFunctor.Coalt

type LnCoapplicative w = (LnCopointed w, LnCoapply w)

type LnCoalternative w = (LnCoapplicative w, LnEmpty w, LnCoalt w)

