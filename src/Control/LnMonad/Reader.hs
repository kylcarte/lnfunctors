{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Control.LnMonad.Reader where

import Data.LnFunctor
import Data.LnFunctor.Apply

newtype LnReader
  (c :: (k -> *) -> k -> k -> Constraint)
  (r :: k -> *)
  (i :: k)
  (j :: k)
  (a :: *) = LnReader
  { unLnReader :: r i -> a
  }

instance IxFunctor (LnReader c r) where
  imap f (LnReader m) = LnReader $ f . m

instance LnFunctor (LnReader c r) where
  type Linkable (LnReader c r) i j = (c r i j)
  type Link (LnReader c r) i j = r i -> r j
  preLink  f (LnReader jm) = LnReader $ jm . f
  postLink f (LnReader im) = LnReader im

instance LnApply (LnReader c r) where
  lap ljk (LnReader imf) (LnReader kma) = LnReader go
    where
    go ri = f a
      where
      f = imf ri
      a = kma (undefined :: r k) -- ???

