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

module Control.LnMonad.List.Reader where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Control.LnApplicative
import Type.Families
import Data.Proxy

newtype LReader
  (i :: [*])
  (j :: [*])
  (a :: *) = LReader
  { unLReader :: List i -> a
  }

instance IxFunctor LReader where
  imap f (LReader mi) = LReader $ f . mi

instance LnFunctor LReader where
  type L LReader i k = IsSub i k
  type R LReader j l = ()
  weaken (LReader (m :: List i -> a)) = LReader $ \lk ->
    case subProof (Proxy :: Proxy i) lk of
      SubProof li -> m li
  strengthen (LReader m) = LReader m
  lmap f = imap f . stretch

instance LnInitial LReader where
  type Init LReader i j = ()

instance LnApply LReader where
  type Link LReader i j k l h m =
    (IsSub i h, IsSub k h)
  lap (LReader (fi :: List i -> a -> b)) (LReader (ak :: List k -> a))
    = LReader $ \lh -> case (subProof pi lh,subProof pk lh) of
      (SubProof li,SubProof lk) -> fi li $ ak lk
    where
    pi = Proxy :: Proxy i
    pk = Proxy :: Proxy k

instance LnApplicative LReader where
  lpure = LReader . const

instance LnBind LReader where
  lbind (LReader (ai :: List i -> a)) (fk :: a -> LReader k l b)
    = LReader $ \lh -> case (subProof pi lh,subProof pk lh) of
      (SubProof li,SubProof lk) -> unLReader (fk $ ai li) lk
    where
    pi = Proxy :: Proxy i
    pk = Proxy :: Proxy k

ask :: LReader i i (List i)
ask = LReader $ \li -> li

local :: (List i -> List k) -> LReader k l a -> LReader i l a
local f (LReader ak) = LReader $ ak . f

reader :: (List i -> a) -> LReader i j a
reader = LReader

