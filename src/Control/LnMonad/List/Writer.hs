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
import Type.Families
import Data.Proxy
import Control.Arrow (first)

newtype WriterLn
  (i :: [*])
  (j :: [*])
  (a :: *) = WriterLn
  { unWriterLn :: (List j,a)
  }

instance IxFunctor WriterLn where
  imap f (WriterLn m) = WriterLn $ fmap f m

instance LnFunctor WriterLn where
  type L WriterLn i k = ()
  type R WriterLn j l = IsSub l j
  weaken (WriterLn p) = WriterLn p
  strengthen (WriterLn (lj,a) :: WriterLn i j a) = WriterLn go
    where
    go :: forall l. IsSub l j => (List l,a)
    go = case subProof pl lj of
      SubProof ll -> (ll,a)
      where
      pl = Proxy :: Proxy l

instance LnInitial WriterLn where
  type Init WriterLn i j = j ~ '[]

instance LnPointed WriterLn where
  lreturn a = WriterLn (Nil,a)

instance LnApply WriterLn where
  type Link WriterLn i j k l h m = IsUnion j l m
  lap (WriterLn ((lj :: List j),(f :: a -> b)))
      (WriterLn ((ll :: List l),(a :: a)))
    = WriterLn (union lj ll,f a)

instance LnBind WriterLn where
  lbind (WriterLn ((lj :: List j),(a :: a)))
        (f :: a -> WriterLn k l b)
    = WriterLn go
    where
    go :: forall m. IsUnion j l m => (List m,b)
    go = (union lj ll,b)
      where
      WriterLn (ll,b) = f a

writer :: (List j,a) -> WriterLn i j a
writer = WriterLn

tell :: List j -> WriterLn i j ()
tell lj = WriterLn (lj,())

listen :: WriterLn i j a -> WriterLn i j (List j,a)
listen (WriterLn p@(lj,a)) = WriterLn (lj,p)

listens :: (List j -> b) -> WriterLn i j a -> WriterLn i j (a,b)
listens f (WriterLn (lj,a)) = WriterLn (lj,(a,f lj))

pass :: WriterLn i j (a,List j -> List k) -> WriterLn i k a
pass (WriterLn (lj,(a,f))) = WriterLn (f lj,a)

censor :: (List j -> List k) -> WriterLn i j a -> WriterLn i k a
censor f (WriterLn (lj,a)) = WriterLn (f lj,a)

