{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor
  ( module Data.LnFunctor
  , module Data.IxFunctor
  ) where

import Data.IxFunctor

import GHC.Exts (Constraint)

class IxFunctor f => LnFunctor f where
  type Linkable f (i :: k) (j :: k) :: Constraint
  preLink  :: Linkable f i j => f j k a -> f i k a
  postLink :: Linkable f j k => f i j a -> f i k a

type WithLink c f = (c f,LnFunctor f)
type Link c f i j = (WithLink c f, Linkable f i j)

type family LinksTo f ijs :: Constraint where
  LinksTo f '[]             = ()
  LinksTo f ('(i,j) ': ijs) = (Linkable f i j, LinksTo f ijs)

type Links c f ijs = (WithLink c f, LinksTo f ijs)

