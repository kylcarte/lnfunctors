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

module Control.LnMonad.List.Writer where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Control.LnApplicative
import Type.Families
import Data.Proxy

newtype LWriter
  (i :: [*])
  (j :: [*])
  (a :: *) = LWriter
  { unLWriter :: (List j,a)
  }

instance IxFunctor LWriter where
  imap f (LWriter m) = LWriter $ fmap f m

instance LnFunctor LWriter where
  type L LWriter i k = ()
  type R LWriter j l = IsSub l j
  weaken (LWriter p) = LWriter p
  strengthen (LWriter (lj,a) :: LWriter i j a) = LWriter go
    where
    go :: forall l. IsSub l j => (List l,a)
    go = case subProof pl lj of
      SubProof ll -> (ll,a)
      where
      pl = Proxy :: Proxy l

instance LnInitial LWriter where
  type Init LWriter i j = j ~ '[]

instance LnApply LWriter where
  type Link LWriter i j k l h m = IsUnion j l m
  lap (LWriter ((lj :: List j),(f :: a -> b)))
      (LWriter ((ll :: List l),(a :: a)))
    = LWriter (union lj ll,f a)

instance LnApplicative LWriter where
  lpure a = LWriter (Nil,a)

instance LnBind LWriter where
  lbind (LWriter ((lj :: List j),(a :: a)))
        (f :: a -> LWriter k l b)
    = LWriter go
    where
    go :: forall m. IsUnion j l m => (List m,b)
    go = (union lj ll,b)
      where
      LWriter (ll,b) = f a

writer :: (List j,a) -> LWriter i j a
writer = LWriter

tell :: List j -> LWriter i j ()
tell lj = LWriter (lj,())

listen :: LWriter i j a -> LWriter i j (List j,a)
listen (LWriter p@(lj,a)) = LWriter (lj,p)

listens :: (List j -> b) -> LWriter i j a -> LWriter i j (a,b)
listens f (LWriter (lj,a)) = LWriter (lj,(a,f lj))

pass :: LWriter i j (a,List j -> List k) -> LWriter i k a
pass (LWriter (lj,(a,f))) = LWriter (f lj,a)

censor :: (List j -> List k) -> LWriter i j a -> LWriter i k a
censor f (LWriter (lj,a)) = LWriter (f lj,a)

