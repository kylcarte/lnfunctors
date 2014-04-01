{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Plus where

import Data.LnFunctor
import Data.IxFunctor.Plus (IxPlus)
import qualified Data.IxFunctor.Plus as I

type LnPlus m = (IxPlus m, LnFunctor m)
type PlusLinks m ijs = (IxPlus m, LinksTo  m ijs)

