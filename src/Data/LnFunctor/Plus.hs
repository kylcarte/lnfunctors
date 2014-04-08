{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Plus where

import Data.LnFunctor

class LnFunctor f => LnPlus f where
  type Plus f i j k l h m :: Constraint
  lplus :: Plus f i j k l h m => f i j a -> f k l a -> f h m a

type IPlus f i j = Plus f i j i j i j

iplus :: (LnPlus f, IPlus f i j) => f i j a -> f i j a -> f i j a
iplus = lplus

