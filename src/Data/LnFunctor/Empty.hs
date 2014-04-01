{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Empty where

import Data.LnFunctor
import Data.IxFunctor.Empty (IxEmpty)
import qualified Data.IxFunctor.Empty as I

type LnEmpty m = WithLink IxEmpty m
type EmptyLinks m ijs = Links IxEmpty m ijs

{-

  iempty :: f i i a

-}

