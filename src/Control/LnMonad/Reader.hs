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
import Type.Families
import Data.Proxy

newtype ReaderLn
  (r :: ix -> *)
  (i :: ix)
  (j :: ix)
  (a :: *) = ReaderLn
  { unReaderLn :: r i -> a
  }

instance IxFunctor (ReaderLn r) where
  imap f (ReaderLn mi) = ReaderLn $ f . mi

instance LnFunctor (ReaderLn r) where
  type LinkL (ReaderLn r) i j =
    ( Inject (R r) i j (r j) (r i)
    , Inject (R r) j i (r i) (r j)
    )
  type LinkR (ReaderLn r) i j = ()
  type LinkI (ReaderLn r) i j = ()
  weaken (ReaderLn m) = ReaderLn $ m . injEnv
  strengthen (ReaderLn m) = ReaderLn m
  lmap f = imap f . stretch

injEnv :: forall r i j. Inject (R r) i j (r j) (r i) => r j -> r i
injEnv = inject pr pi pj
  where
  pr = Proxy :: Proxy (R r)
  pi = Proxy :: Proxy i
  pj = Proxy :: Proxy j

-- TNe (r i) (r j) => 
class R (r :: ix -> *) (i :: ix) (j :: ix) where
  transEnv :: r j -> r i

-- Register the injection
instance TNe (r i) (r j) => InjectNe (R r) i j (r j) (r i) where
  inject_ Proxy = transEnv

instance LnInitial (ReaderLn r) where
  type Init (ReaderLn r) i j = ()

instance LnPointed (ReaderLn r) where
  lreturn = ReaderLn . const

instance LnApply (ReaderLn r) where
  lap (ReaderLn mf) (ReaderLn ma) = ReaderLn $ \ri ->
    mf ri $ ma $ injEnv ri

instance LnBind (ReaderLn r) where
  lbind (ReaderLn mij) f = ReaderLn go
    where
    go ri = mkl $ injEnv ri
      where
      a = mij ri
      ReaderLn mkl = f a

{-
data List as where
  Nil  :: List '[]
  (:*) :: a -> List as -> List (a ': as)

instance TypeIxMonoid List where
  type MonoidKind List = ListK
  tmempty  = Nil
  tmappend as bs = case as of
    Nil      -> bs
    a :* as' -> a :* tmappend as' bs

instance TypeIxMonoid1 List where
  type Munit List a = List '[a]
  tmunit = (:* Nil)
-}

