{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Control.LnMonad.Infer where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Control.LnApplicative
import Type.Families
import Data.Proxy
import Control.Arrow (first)

data Eqv a b where
  Refl :: Eqv a a

type (:=:) = Eqv
infixr 3 :=:

newtype Infer as cs a = Infer
  { runInfer :: List as -> (List cs,a)
  }

instance IxFunctor Infer where
  imap f (Infer m) = Infer $ fmap f . m

instance LnFunctor Infer where
  type L Infer i k = IsSub i k
  type R Infer j l = IsSub l j
  weaken     (Infer ai) = Infer $        ai . sub
  strengthen (Infer ai) = Infer $ first sub . ai

instance LnInitial Infer where
  type Init Infer i j = IsSub j i

instance LnApply Infer where
  type Link Infer i j k l h m
    = (h ~ i, IsUnion i j k, l ~ m)
  lap (Infer (fij :: List i -> (List j,a -> b)))
      (Infer (akl :: List k -> (List l,a))) = Infer go
    where
    go :: List i -> (List l,b)
    go li = (ll,f a)
      where
      (lj,f) = fij   li
      (ll,a) = akl $ union li lj

instance LnApplicative Infer where
  lpure a = Infer $ \li -> (sub li,a)

instance LnBind Infer where
  lbind (Infer (aij :: List i -> (List j,a)))
        (f :: (a -> Infer k l b)) = Infer go
    where
    go :: List i -> (List l,b)
    go li = bkl $ union li lj
      where
      (lj,a)    = aij li
      Infer bkl = f a

