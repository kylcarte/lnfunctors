{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Apply where

import Data.LnFunctor
import Data.IxFunctor.Apply (IxApply)
import qualified Data.IxFunctor.Apply as I

type LnApply m = WithLink IxApply m
type ApplyLinks m ijs = Links IxApply m ijs

{-
class IxFunctor m => IxApply m where
  iap    :: m i j (a -> b) -> m j k a -> m i k b

ithenL :: IxApply m => m i j a -> m j k b -> m i k a
ithenL = iliftA2 const

ithenR :: IxApply m => m i j a -> m j k b -> m i k b
ithenR = iliftA2 (const id)

(<**>) :: IxApply m => m i j (a -> b) -> m j k a -> m i k b
(<**>) = iap
infixl 4 <**>

(<**) :: IxApply m => m i j a -> m j k b -> m i k a
(<**) = ithenL
infixl 4 <**

(**>) :: IxApply m => m i j a -> m j k b -> m i k b
(**>) = ithenR
infixl 4 **>

iliftA :: IxApply m => (a -> b) -> m i j a -> m i j b
iliftA = (<$$>)

iliftA2 :: IxApply m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftA2 f a b = f <$$> a <**> b

iliftA3 :: IxApply m => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
iliftA3 f a b c = f <$$> a <**> b <**> c
-}

