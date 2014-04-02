{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor where

import Data.IxFunctor         (IxFunctor (..))
import Data.IxFunctor.Apply   (IxApply   (..))
import Data.IxFunctor.Empty   (IxEmpty   (..))
import Data.IxFunctor.Alt
import Data.IxFunctor.Pointed (IxPointed (..))
import Data.IxFunctor.Bind    (IxBind    (..))
import qualified Data.IxFunctor.Bind as Bind

import GHC.Exts (Constraint)

type FunctorLink  f i j = (LnFunctor f, Link f i j)
type Rfl
  (c :: (ix -> ix -> * -> *) -> ix -> ix -> k)
  (f ::  ix -> ix -> * -> *)
  (i ::  ix)
  = (c f i i :: k)

class LnFunctor f => LnInitial (f :: ix -> ix -> * -> *) where
  type Init f :: ix -> ix -> Constraint
class LnFunctor f => LnTerminal (f :: ix -> ix -> * -> *) where
  type Term f :: ix -> ix -> Constraint

class LnFunctor f => LnJoin (f :: ix -> ix -> * -> *) where
  type JoinEn f :: ix -> ix -> Constraint
  type JoinEx f :: ix -> ix -> Constraint
class LnFunctor f => LnSplit (f :: ix -> ix -> * -> *) where
  type SplitEn f :: ix -> ix -> Constraint
  type SplitEx f :: ix -> ix -> Constraint

-- Functor {{{

class LnFunctor (f :: ix -> ix -> * -> *) where
  type Link f :: ix -> ix -> Constraint
  weaken     :: Link f i j => f j k a -> f i k a
  strengthen :: Link f j k => f i j a -> f i k a
  stretch    :: (Link f i j, Link f k l) => f j k a -> f i l a
  stretch = strengthen . weaken
  -- mapping under LnFunctor, generalized from IxFunctor 'imap'
  lmap :: Link f i j => (a -> b) -> f j k a -> f i k b
  lmap = lmapWeaken
  -- map with weaken
  lmapWeaken :: Link f i j => (a -> b) -> f j k a -> f i k b
  default lmapWeaken :: (IxFunctor f, FunctorLink f i j)
    => (a -> b) -> f j k a -> f i k b
  lmapWeaken f = weaken . imap f
  -- map with strengthen
  lmapStrengthen :: Link f j k => (a -> b) -> f i j a -> f i k b
  default lmapStrengthen :: (IxFunctor f, FunctorLink f j k)
    => (a -> b) -> f i j a -> f i k b
  lmapStrengthen f = strengthen . imap f

(<$$>) :: (LnFunctor f, Rfl Link f i) => (a -> b) -> f i j a -> f i j b
(<$$>) = lmap
infixl 4 <$$>

(<$$) :: (LnFunctor f, Rfl Link f i) => a -> f i j b -> f i j a
a <$$ f = const a <$$> f
infixl 4 <$$

($$>) :: (LnFunctor f, Rfl Link f i) => f i j b -> a -> f i j a
($$>) = flip (<$$)

lvoid :: (LnFunctor f, Rfl Link f i) => f i j a -> f i j ()
lvoid = (() <$$)

-- }}}

-- Empty {{{

class (LnFunctor f, LnInitial f) => LnEmpty f where
  lempty :: Init f i j => f i j a
  default lempty :: (IxEmpty f, FunctorLink f i j)
    => f i j a
  lempty = weaken $ iempty

rempty :: (LnEmpty f, Rfl Init f i) => f i i a
rempty = lempty

-- }}}

-- Pointed {{{

class (LnFunctor m, LnInitial m) => LnPointed (m :: ix -> ix -> * -> *) where
  lreturn :: Init m i j => a -> m i j a
  default lreturn :: (IxPointed m, FunctorLink m i j)
    => a -> m i j a
  lreturn = weaken . ireturn

rreturn :: (LnPointed m, Rfl Init m i)
  => a -> m i i a
rreturn = lreturn

-- }}}

-- Apply {{{

class LnFunctor m => LnApply m where
  lap :: Link m j k => m i j (a -> b) -> m k l a -> m i l b
  default lap :: (IxApply m, FunctorLink m j k)
    => m i j (a -> b) -> m k l a -> m i l b
  lap f = iap f . weaken

rliftA :: (LnApply m, Rfl Link m i)
  => (a -> b) -> m i j a -> m i j b
rliftA = (<$$>)

(<**>) :: (LnApply m, Link m j k)
  => m i j (a -> b) -> m k l a -> m i l b
(<**>) = lap
infixl 4 <**>

rliftA2 :: (LnApply m, Link m j k, Rfl Link m i)
  => (a -> b -> c) -> m i j a -> m k l b -> m i l c
rliftA2 f a b = f <$$> a <**> b

rliftA3 :: (LnApply m, Link m j k, Link m l n, Rfl Link m i)
  => (a -> b -> c -> d)
  -> m i j a -> m k l b -> m n o c -> m i o d
rliftA3 f a b c = f <$$> a <**> b <**> c

rthenL :: (LnApply m, Link m j k, Rfl Link m i)
  => m i j a -> m k l b -> m i l a
rthenL = rliftA2 const

rthenR :: (LnApply m, Link m j k, Rfl Link m i)
  => m i j a -> m k l b -> m i l b
rthenR = rliftA2 (const id)

(<**) :: (LnApply m, Link m j k, Rfl Link m i)
  => m i j a -> m k l b -> m i l a
(<**) = rthenL
infixl 4 <**

(**>) :: (LnApply m, Link m j k, Rfl Link m i)
  => m i j a -> m k l b -> m i l b
(**>) = rthenR
infixl 4 **>

-- }}}

-- Alt {{{

class (LnFunctor m, LnJoin m) => LnAlt m where
  laltL    :: JoinEn m i j => m i k a -> m j k a -> m i k a
  laltR    :: JoinEn m i j => m i k a -> m j k a -> m j k a
  lfunnelL :: JoinEx m j k => m i j a -> m i k a -> m i j a
  lfunnelR :: JoinEx m j k => m i j a -> m i k a -> m i k a

-- }}}

{-
-- Bind {{{

class LnApply m => LnBind m where
  lbind :: Link m j k => m i j a -> (a -> m k l b) -> m i l b

rbind :: (LnBind m, Reflex Link m j) => m i j a -> (a -> m j k b) -> m i k b
rbind = lbind

ljoin :: (LnBind m, Link m j k) => m i j (m k l a) -> m i l a
ljoin = flip lbind id

(>>>=) :: (LnBind m, Link m j k) => m i j a -> (a -> m k l b) -> m i l b
(>>>=) = lbind
infixr 1 >>>=

(=<<<) :: (LnBind m, Link m j k) => (a -> m k l b) -> m i j a -> m i l b
(=<<<) = flip (>>>=)
infixl 1 =<<<

(>>>) :: (LnBind m, Link m j k) => m i j a -> m k l b -> m i l b
m1 >>> m2 = m1 >>>= const m2
infixl 1 >>>

(>>=>) :: (LnBind m, Link m j k) => (a -> m i j b) -> (b -> m k l c) -> a -> m i l c
(f >>=> g) x = f x >>>= g
infixr 1 >>=>

(<=<<) :: (LnBind m, Link m j k) => (b -> m k l c) -> (a -> m i j b) -> a -> m i l c
(f <=<< g) x = g x >>>= f
infixr 1 <=<<

-- }}}

type LnMonad  m = (LnPointed m, LnBind m)

strengthenM :: (LnMonad m, Link m j k, Reflex Init m k)
  => m i j b -> m i k b
strengthenM = (>>>= rreturn)

lapLnMonad :: (LnMonad m, Link m j k, Reflex Link m l, Reflex Init m l)
  => m i j (a -> b) -> m k l a -> m i l b
lapLnMonad f x =
  f >>>= \f' ->
  x >>>= \x' ->
  rreturn $ f' x'

lliftM :: LnMonad m => (a -> b) -> m i j a -> m i j b
lliftM = lliftA

iliftM2 :: LnMonad m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 = iliftA2

iliftM3 :: LnMonad m => (a -> b -> c -> d)
-> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 = iliftA3

isequence :: LnMonad m => [m i i a] -> m i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: LnMonad m => (a -> m i i b) -> [a] -> m i i [b]
imapM f = isequence . map f

isequence_ :: LnMonad m => [m i i a] -> m i i ()
isequence_ = foldr (>>>) $ ireturn ()

imapM_ :: LnMonad m => (a -> m i i b) -> [a] -> m i i ()
imapM_ f = isequence_ . map f
-}

