{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.LnFunctor.Mono where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Data.Proxy
import Type.Families

class IxMono (f :: k) (t :: Kind ix) | f -> t where
  type Mono f (t :: Kind ix) (i :: ix) (j :: ix) :: * -> *

data WrappedMono (f :: k) (t :: Kind ix) :: ix -> ix -> * -> * where
  WrapMono :: IxMono f (t :: Kind ix)
       => { monoProxy  :: IxP f t (i :: ix) (j :: ix)
          , unwrapMono :: Mono f t (i :: ix) (j :: ix) a
          }
       -> WrappedMono f t i j a

-- Wrapping functions {{{

type IxP f t i j = Proxy (f:*:t:*:i:*:j)

wrapMono_ :: forall f t i j a. IxMono f t => Mono f t i j a -> WrappedMono f t i j a
wrapMono_ = WrapMono (Proxy :: IxP f t i j)

wrapMonoL :: forall f t i j k a.  IxMono f t
  => (Proxy k -> Mono f t k j a)
  -> WrappedMono f t k j a
wrapMonoL f = wrapMono_ $ f (Proxy :: Proxy k)

wrapMonoR :: forall f t i j l a. IxMono f t
  => (Proxy l -> Mono f t i l a)
  -> WrappedMono f t i l a
wrapMonoR f = wrapMono_ $ f (Proxy :: Proxy l)

wrapMono  :: forall f t i j k l a. IxMono f t
  => (IxP f t i j -> Mono f t i j a)
  -> WrappedMono f t i j a
wrapMono  f = wrapMono_ $ f (Proxy :: IxP f t i j)

-- }}}

-- IxFunctor {{{

class IxMono f t => IxFunctorMono f t | f -> t where
  imapMono :: (a -> b) -> Mono f t i j a -> IxP f t i j -> Mono f t i j b

instance IxFunctorMono f t => IxFunctor (WrappedMono f t) where
  imap f (WrapMono p m :: WrappedMono f t i j a) = wrapMono $ imapMono f m

-- }}}

-- LnFunctor {{{

class IxFunctorMono f t => LnFunctorMono f (t :: Kind ix) | f -> t where
  type LMono f (t :: Kind ix) (i :: ix) (k :: ix) :: Constraint
  type RMono f (t :: Kind ix) (j :: ix) (l :: ix) :: Constraint
  weakenMono     :: LMono f t i k => IxP f t i j -> Mono f t i j a -> Proxy k -> Mono f t k j a
  strengthenMono :: RMono f t j l => IxP f t i j -> Mono f t i j a -> Proxy l -> Mono f t i l a

instance LnFunctorMono f t => LnFunctor (WrappedMono f t) where
  type L (WrappedMono f t) i k = LMono f t i k
  type R (WrappedMono f t) j l = RMono f t j l
  weaken (WrapMono p m :: WrappedMono f t i j a) = go
    where
    go :: forall k. LMono f t i k => WrappedMono f t k j a
    go = wrapMonoL $ weakenMono p m
  strengthen (WrapMono p m :: WrappedMono f t i j a) = go
    where
    go :: forall l. RMono f t j l => WrappedMono f t i l a
    go = wrapMonoR $ strengthenMono p m

-- }}}

-- LnApply {{{

class LnFunctorMono f t => LnApplyMono f (t :: Kind ix) | f -> t where
  type LinkMono f (t :: Kind ix)
    (i :: ix) (j :: ix)
    (k :: ix) (l :: ix)
    (h :: ix) (m :: ix) :: Constraint
  lapMono :: LinkMono f t i j k l h m
    => IxP f t i j
    -> IxP f t k l
    -> Mono f t i j (a -> b)
    -> Mono f t k l a
    -> IxP f t h m
    -> Mono f t h m b

instance LnApplyMono f t => LnApply (WrappedMono f t) where
  type Link (WrappedMono f t) i j k l h m = LinkMono f t i j k l h m
  lap (WrapMono p1 m1 :: WrappedMono f t i j (a -> b)) (WrapMono p2 m2 :: WrappedMono f t k l a) = go
    where
    go :: forall h m. LinkMono f t i j k l h m => WrappedMono f t h m b
    go = wrapMono $ lapMono p1 p2 m1 m2

-- }}}

-- LnBind {{{

class LnApplyMono f t => LnBindMono f (t :: Kind ix) | f -> t where
  lbindMono :: LinkMono f t i j k l h m
    => IxP f t i j
    -> IxP f t k l
    -> Mono f t i j a
    -> (a -> Mono f t k l b)
    -> IxP f t h m
    -> Mono f t h m b

instance LnBindMono f t => LnBind (WrappedMono f t) where
  lbind (WrapMono p1 m1 :: WrappedMono f t i j a) (f :: a -> WrappedMono f t k l b) = go
    where
    go :: forall h m. LinkMono f t i j k l h m => WrappedMono f t h m b
    go = wrapMono $ lbindMono p1 p2 m1 $ unwrapMono . f
      where
      p2  = Proxy :: IxP f t k l

-- }}}

