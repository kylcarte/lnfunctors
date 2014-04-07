{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Control.LnMonad.List.State where

import Data.LnFunctor
import Type.Families
import Data.Proxy
import Control.Arrow (first)

newtype StateLn
  (i :: [*])
  (j :: [*])
  (a :: *) = StateLn
  { unStateLn :: List i -> (List j,a)
  }

instance IxFunctor StateLn where
  imap f (StateLn m) = StateLn $ fmap f . m

instance LnFunctor StateLn where
  type L StateLn i k = IsSub i k
  type R StateLn j l = IsSub l j
  weaken (StateLn (m :: List i -> (List j,a))) = StateLn go
    where
    go :: forall k. IsSub i k => List k -> (List j,a)
    go lk = case subProof pi lk of
      SubProof li -> m li
      where
      pi = Proxy :: Proxy i
  strengthen (StateLn (m :: List i -> (List j,a))) = StateLn go
    where
    go :: forall l. IsSub l j => List i -> (List l,a)
    go li = case subProof pl lj of
      SubProof ll-> (ll,a)
      where
      pl = Proxy :: Proxy l
      (lj,a) = m li

instance LnInitial StateLn where
  type Init StateLn i j = i ~ j

instance LnPointed StateLn where
  lreturn a = StateLn $ \li -> (li,a)

instance LnApply StateLn where
  type Link StateLn i j k l h m =
    ( IsSub i h
    , IsSub m l
    , IsSub k j
    )
  lap (StateLn (fi :: List i -> (List j,a -> b)))
      (StateLn (ak :: List k -> (List l,a)))
    = StateLn go
    where
    go :: forall h m. (IsSub i h, IsSub m l) => List h -> (List m,b)
    go lh = case subProof pi lh of
      SubProof li -> case subProof pk lj of
        SubProof lk -> case subProof pm ll of
          SubProof lm -> (lm,f a)
          where
          (ll,a) = ak lk
        where
        (lj,f) = fi li
      where
      pi = Proxy :: Proxy i
      pk = Proxy :: Proxy k
      pm = Proxy :: Proxy m

instance LnBind StateLn where
  lbind (StateLn (ai :: List i -> (List j,a)))
      (f :: a -> StateLn k l b)
    = StateLn go
    where
    go :: forall h m. (IsSub i h, IsSub m l) => List h -> (List m,b)
    go lh = case subProof pi lh of
      SubProof li -> case subProof pk lj of
        SubProof lk -> case subProof pm ll of
          SubProof lm -> (lm,b)
          where
          (ll,b) = bk lk
        where
        (lj,a) = ai li
        StateLn bk = f a
      where
      pi = Proxy :: Proxy i
      pk = Proxy :: Proxy k
      pm = Proxy :: Proxy m

