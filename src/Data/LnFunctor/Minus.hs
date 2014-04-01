{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Minus where

import Data.LnFunctor
import Data.IxFunctor.Minus (IxMinus)
import qualified Data.IxFunctor.Minus as I

type LnMinus m = (IxMinus m, LnFunctor m)
type MinusLinks m ijs = (IxMinus m, LinksTo  m ijs)

