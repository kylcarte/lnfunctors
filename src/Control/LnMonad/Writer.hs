{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Control.LnMonad.Writer where

import Data.LnFunctor
import Type.Families

newtype WriterLn
  (w :: ix -> *)
  (i :: ix)
  (j :: ix)
  (a :: *) = WriterLn
  { unWriterLn :: (a,w j)
  }

instance IxFunctor (WriterLn r) where
  imap f (WriterLn (a,wj)) = WriterLn (f a,wj)

instance LnFunctor (WriterLn r) where
  type LinkL (WriterLn r) i j = ()
  type LinkR (WriterLn r) i j = Sym WriterLink r i j
  type LinkI (WriterLn r) i j = ()
  weaken (WriterLn m) = WriterLn m
  strengthen (WriterLn (a,wj))   = WriterLn (a,mapLog $ wj)
  lmap f = imap f . stretch

class WriterLink (w :: ix -> *) (i :: ix) (j :: ix) where
  mapLog :: w i -> w j

instance TypeIxMonoid w => LnInitial (WriterLn w) where
  type Init (WriterLn w) i j = j ~ Mempty (MonoidKind w)

instance TypeIxMonoid w => LnPointed (WriterLn w) where
  lreturn a = WriterLn (a,tmempty)

{-
instance IxMonoid w => LnApply (WriterLn w) where
  lap (WriterLn (f,wj)) (WriterLn (a,wl)) = WriterLn (f a, imappend (mapLog wj) wl)

instance IxMonoid w => LnBind (WriterLn w) where
  lbind (WriterLn (a,wj)) f = WriterLn (b,imappend (mapLog wj) wl)
    where
    WriterLn (b,wl) = f a
-}

