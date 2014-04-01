{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Coalt where

import Data.LnFunctor
import Data.IxFunctor.Coalt (IxCoalt)
import qualified Data.IxFunctor.Coalt as I

type LnCoalt m = WithLink IxCoalt m
type CoaltLinks m ijs = Links IxCoalt m ijs

{-

class IxCoapply w => IxCoalt w where
  icoalt  :: w i j a -> (w i j a,w i j a)
  icosome :: w i j a -> [w i j a]
  icomany :: w i j a -> [w i j a]

-}

