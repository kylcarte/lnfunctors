{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor
  ( module Data.LnFunctor
  , module Data.IxFunctor
  , Constraint
  ) where

import Data.IxFunctor

import GHC.Exts (Constraint)

class IxFunctor f => LnFunctor f where
  type Linkable f (i :: k) (j :: k) :: Constraint
  type Link f (i :: k) (j :: k) :: *
  preLink  :: Linkable f i j => Link f i j -> f j k a -> f i k a
  postLink :: Linkable f j k => Link f j k -> f i j a -> f i k a

type WithLink  f i j r = Linkable f i j => Link f i j -> r
type WithLinks f ijs r = LinksTo f ijs  => WithLinks_ f ijs r

type family WithLinks_ f ijs r where
  WithLinks_ f '[] r = r
  WithLinks_ f ('(i,j) ': ijs) r = Link f i j -> WithLinks_ f ijs r

type family LinksTo f ijs :: Constraint where
  LinksTo f '[]             = ()
  LinksTo f ('(i,j) ': ijs) = (Linkable f i j, LinksTo f ijs)

class Transform a b where
  transform :: a -> b

