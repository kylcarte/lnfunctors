{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Extend where

import Data.LnFunctor
import Data.IxFunctor.Extend (IxExtend)
import qualified Data.IxFunctor.Extend as I

type LnExtend m = WithLink IxExtend m
type ExtendLinks m ijs = Links IxExtend m ijs

{-

class IxFunctor w => IxExtend w where
  iextend :: w i k a -> (w j k a -> b) -> w i j b

iduplicate :: IxExtend w => w i k a -> w i j (w j k a)
iduplicate w = iextend w id

(=>>>) :: IxExtend w => w i k a -> (w j k a -> b) -> w i j b
(=>>>) = iextend
infixl 1 =>>>

(<<<=) :: IxExtend w => (w j k a -> b) -> w i k a -> w i j b
(<<<=) = flip (=>>>)
infixr 1 <<<=

(=>>=) :: IxExtend w => (w k l a -> b) -> (w j k b -> c) -> w i l a -> w i j c
(f =>>= g) w = w =>>> f =>>> g
infixl 1 =>>=

(=<<=) :: IxExtend w => (w j k b -> c) -> (w k l a -> b) -> w i l a -> w i j c
(=<<=) = flip (=>>=)
infixr 1 =<<=

-}

