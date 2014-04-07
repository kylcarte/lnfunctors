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
import Type.Families
import Data.Proxy

newtype ReaderLn
  (i :: [*])
  (j :: [*])
  (a :: *) = ReaderLn
  { unReaderLn :: List i -> a
  }

instance IxFunctor ReaderLn where
  imap f (ReaderLn mi) = ReaderLn $ f . mi

instance LnFunctor ReaderLn where
  type L ReaderLn i k = IsSub i k
  type R ReaderLn j l = ()
  weaken (ReaderLn (m :: List i -> a)) = ReaderLn $ \lk ->
    case subProof (Proxy :: Proxy i) lk of
      SubProof li -> m li
  strengthen (ReaderLn m) = ReaderLn m
  lmap f = imap f . stretch

instance LnInitial ReaderLn where
  type Init ReaderLn i j = ()

instance LnPointed ReaderLn where
  lreturn = ReaderLn . const

instance LnApply ReaderLn where
  type Link ReaderLn i j k l h m =
    (IsSub i h, IsSub k h)
  lap (ReaderLn (fi :: List i -> a -> b)) (ReaderLn (ak :: List k -> a))
    = ReaderLn $ \lh -> case (subProof pi lh,subProof pk lh) of
      (SubProof li,SubProof lk) -> fi li $ ak lk
    where
    pi = Proxy :: Proxy i
    pk = Proxy :: Proxy k

instance LnBind ReaderLn where
  lbind (ReaderLn (ai :: List i -> a)) (fk :: a -> ReaderLn k l b)
    = ReaderLn $ \lh -> case (subProof pi lh,subProof pk lh) of
      (SubProof li,SubProof lk) -> unReaderLn (fk $ ai li) lk
    where
    pi = Proxy :: Proxy i
    pk = Proxy :: Proxy k

ask :: ReaderLn i i (List i)
ask = ReaderLn $ \li -> li

local :: (List i -> List k) -> ReaderLn k l a -> ReaderLn i l a
local f (ReaderLn ak) = ReaderLn $ ak . f

reader :: (List i -> a) -> ReaderLn i j a
reader = ReaderLn

