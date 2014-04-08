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

module Control.LnMonad.Reader where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Control.LnApplicative
import Type.Families

newtype Reader
  (r :: k -> *)
  (i :: k)
  (j :: k)
  (a :: *) = Reader
  { unReader :: r i -> a
  }

class Forget (r :: k -> *) (i :: k) (j :: k) where
  forget :: r j -> r i

instance IxFunctor (Reader r) where
  imap f (Reader mi) = Reader $ f . mi

instance LnFunctor (Reader r) where
  type L (Reader r) i k = Forget r i k
  type R (Reader r) j l = ()
  weaken (Reader (m :: r i -> a)) =
    Reader $ m . forget
  strengthen (Reader m) = Reader m
  lmap f = imap f . stretch

instance LnInitial (Reader r) where
  type Init (Reader r) i j = ()

instance LnApply (Reader r) where
  type Link (Reader r) i j k l h m =
    (Forget r i h, Forget r k h)
  lap (Reader (fi :: r i -> a -> b)) (Reader (ak :: r k -> a))
    = Reader $ \rh -> fi (forget rh) $ ak (forget rh)

instance LnApplicative (Reader r) where
  lpure = Reader . const

instance LnBind (Reader r) where
  lbind (Reader (ai :: r i -> a)) (fk :: a -> Reader r k l b)
    = Reader $ \rh -> unReader (fk $ ai $ forget rh) $ forget rh

ask :: Reader r i i (r i)
ask = Reader id

local :: (r i -> r k) -> Reader r k l a -> Reader r i l a
local f (Reader ak) = Reader $ ak . f

reader :: (r i -> a) -> Reader r i j a
reader = Reader

