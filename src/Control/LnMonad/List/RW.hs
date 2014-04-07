{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Control.LnMonad.List.RW where

import Data.LnFunctor
import Type.Families
import Control.Arrow (first)
import Data.Proxy

newtype RW i j a = RW
  { runRW :: List i -> (List j,a)
  }

instance IxFunctor RW where
  imap f (RW m) = RW $ fmap f . m

instance LnFunctor RW where
  type L RW i k = IsSub i k
  type R RW j l = IsSub l j
  weaken (RW (m :: List i -> (List l,a))) = RW go
    where
    go :: Sub i k => List k -> (List l,a)
    go lk = case subProof pi lk of
      SubProof li  -> m li
      where
      pi = Proxy :: Proxy i
  strengthen (RW (m :: List i -> (List j,a))) = RW go
    where
    go :: forall l. Sub l j => List i -> (List l,a)
    go li = case subProof pl lj of
      SubProof ll -> (ll,a)
      where
      pl = Proxy :: Proxy l
      (lj,a) = m li

instance LnInitial RW where
  type Init RW i j = IsSub j i

instance LnPointed RW where
  lreturn (a :: a) = RW go
    where
    go :: forall i j. Sub j i => List i -> (List j,a)
    go li = case subProof pj li of
      SubProof lj -> (lj,a)
      where
      pj = Proxy :: Proxy j

instance LnApply RW where
  type Link RW i j k l h m =
    ( IsSub i h
    , IsUnion i j k
    , IsSub m l
    )
  lap (RW (fi :: List i -> (List j,a -> b)))
      (RW (ak :: List k -> (List l,a))) = RW go
    where
    go :: forall h m. (Sub i h, Sub m l) => List h -> (List m,b)
    go lh = case subProof pi lh of
      SubProof li -> case subProof pm ll of
        SubProof lm -> (lm,f a)
        where
        (lj,f) = fi li
        (ll,a) = ak $ union li lj
      where
      pi = Proxy :: Proxy i
      pm = Proxy :: Proxy m

instance LnBind RW where
  lbind (RW (ai :: List i -> (List j,a)))
        (f :: a -> RW k l b) = RW go
    where
    go :: forall h m. (Sub i h, Sub m l) => List h -> (List m,b)
    go lh = case subProof pi lh of
      SubProof li -> case subProof pm ll of
        SubProof lm -> (lm,b)
        where
        (lj,a) = ai li
        RW bk  = f a
        (ll,b) = bk $ union li lj
      where
      pi = Proxy :: Proxy i
      pm = Proxy :: Proxy m

