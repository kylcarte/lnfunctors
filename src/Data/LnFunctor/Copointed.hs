{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Copointed where

import Data.LnFunctor
import Data.IxFunctor.Copointed (IxCopointed)
import qualified Data.IxFunctor.Copointed as I

type LnCopointed m = WithLink IxCopointed m
type CopointedLinks m ijs = Links IxCopointed m ijs

{-
  iextract :: w i i a -> a
-}

