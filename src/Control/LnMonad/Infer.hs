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
import Data.LnFunctor.Copointed
import Data.LnFunctor.Coapply
import Type.Families
import Data.Proxy
import Control.Arrow (first)

data Eqv a b where
  Refl :: Eqv a a

type (:=:) = Eqv
infixr 3 :=:

newtype Infer (gs :: [*]) as cs a = Infer
  { runInfer :: List as -> (List cs,a)
  }

instance IxFunctor (Infer gs) where
  imap f (Infer m) = Infer $ fmap f . m

instance LnFunctor (Infer gs) where
  type L (Infer gs) i k = IsSub i k
  type R (Infer gs) j l = IsSub l j
  weaken     (Infer ai) = Infer $        ai . sub
  strengthen (Infer ai) = Infer $ first sub . ai

instance LnInitial (Infer gs) where
  type Init (Infer gs) i j = IsSub j i

instance LnApply (Infer gs) where
  type Link (Infer gs) i j k l h m
    = (h ~ i, IsUnion i j k, l ~ m)
  lap (Infer (fij :: List i -> (List j,a -> b)))
      (Infer (akl :: List k -> (List l,a))) = Infer go
    where
    go :: List i -> (List l,b)
    go li = (ll,f a)
      where
      (lj,f) = fij   li
      (ll,a) = akl $ union li lj

instance LnBind (Infer gs) where
  lbind (Infer (aij :: List i -> (List j,a)))
        (f :: (a -> Infer gs k l b)) = Infer go
    where
    go :: List i -> (List l,b)
    go li = bkl $ union li lj
      where
      (lj,a)    = aij li
      Infer bkl = f a

instance LnTerminal (Infer gs) where
  type Term (Infer gs) i j = (i ~ '[], SetEq j gs)

instance LnCopointed (Infer gs) where
  lextract (Infer aij) = snd $ aij Nil

{-
instance LnCoapply (Infer gs) where 
  type Unlink (Infer gs) h m k l i j = (h ~ i, , l ~ m)
-}

