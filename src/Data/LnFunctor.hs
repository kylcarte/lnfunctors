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
import Data.IxFunctor.Alt     (IxAlt     (..))
import Data.IxFunctor.Pointed (IxPointed (..))
import Data.IxFunctor.Bind    (IxBind    (..))
import Data.IxFunctor.Copointed (IxCopointed (..))

import GHC.Exts (Constraint)

type FunctorLink f i j = (LnFunctor f, Link f i j)
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
  type GatherUp   f :: ix -> ix -> Constraint
  type GatherDown f :: ix -> ix -> Constraint
class LnFunctor f => LnSplit (f :: ix -> ix -> * -> *) where
  type UpSplit   f :: ix -> ix -> Constraint
  type DownSplit f :: ix -> ix -> Constraint

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
  default lmapWeaken :: (IxFunctor f, Link f i j)
    => (a -> b) -> f j k a -> f i k b
  lmapWeaken f = weaken . imap f
  -- map with strengthen
  lmapStrengthen :: Link f j k => (a -> b) -> f i j a -> f i k b
  default lmapStrengthen :: (IxFunctor f, Link f j k)
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
  default lempty :: (IxEmpty f, Link f i j, Init f i j)
    => f i j a
  lempty = weaken $ iempty

rempty :: (LnEmpty f, Rfl Init f i) => f i i a
rempty = lempty

-- }}}

-- Pointed {{{

class (LnFunctor m, LnInitial m) => LnPointed (m :: ix -> ix -> * -> *) where
  lreturn :: Init m i j => a -> m i j a
  default lreturn :: (IxPointed m, Link m i j, Init m i j)
    => a -> m i j a
  lreturn = weaken . ireturn

rreturn :: (LnPointed m, Rfl Init m i)
  => a -> m i i a
rreturn = lreturn

-- }}}

-- Apply {{{

class LnFunctor m => LnApply m where
  lap :: Link m j k => m i j (a -> b) -> m k l a -> m i l b
  default lap :: (IxApply m, Link m j k)
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
  lbranchL :: GatherUp m i j => m i k a -> m j k a -> m i k a
  default lbranchL :: (IxAlt m, Link m i j, GatherUp m i j) => m i k a -> m j k a -> m i k a
  lbranchL m1 m2 = ialt m1 (weaken m2)

  lbranchR :: GatherUp m i j => m i k a -> m j k a -> m j k a
  default lbranchR :: (IxAlt m, Link m j i, GatherUp m i j) => m i k a -> m j k a -> m j k a
  lbranchR m1 m2 = ialt (weaken m1) m2

  lfunnelL :: GatherDown m j k => m i j a -> m i k a -> m i j a
  default lfunnelL :: (IxAlt m, Link m k j, GatherDown m j k) => m i j a -> m i k a -> m i j a
  lfunnelL m1 m2 = ialt m1 (strengthen m2)

  lfunnelR :: GatherDown m j k => m i j a -> m i k a -> m i k a
  default lfunnelR :: (IxAlt m, Link m j k, GatherDown m j k) => m i j a -> m i k a -> m i k a
  lfunnelR m1 m2 = ialt (strengthen m1) m2

(<||>) :: (LnAlt m, Rfl GatherUp m i) => m i j a -> m i j a -> m i j a
(<||>) = lbranchL

-- }}}

-- Bind {{{

class LnApply m => LnBind m where
  lbind :: Link m j k => m i j a -> (a -> m k l b) -> m i l b
  default lbind :: (IxBind m, Link m j k) => m i j a -> (a -> m k l b) -> m i l b
  lbind m f = ibind m $ weaken . f

rbind :: (LnBind m, Rfl Link m j) => m i j a -> (a -> m j k b) -> m i k b
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

-- Monad {{{

type LnMonad  m = (LnPointed m, LnBind m)

strengthenM :: (LnMonad m, Link m j k, Rfl Init m k)
  => m i j b -> m i k b
strengthenM = (>>>= rreturn)

lapLnMonad :: (LnMonad m, Link m j k, Rfl Link m l, Rfl Init m l)
  => m i j (a -> b) -> m k l a -> m i l b
lapLnMonad f x =
  f >>>= \f' ->
  x >>>= \x' ->
  rreturn $ f' x'

rliftM :: (LnMonad m, Rfl Link m i) => (a -> b) -> m i j a -> m i j b
rliftM = rliftA

rliftM2 :: (LnMonad m, Rfl Link m i, Rfl Link m j) => (a -> b -> c) -> m i j a -> m j k b -> m i k c
rliftM2 = rliftA2

rliftM3 :: (LnMonad m, Rfl Link m i, Rfl Link m j, Rfl Link m k) => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
rliftM3 = rliftA3

rsequence :: (LnMonad m, Rfl Link m i, Rfl Init m i) => [m i i a] -> m i i [a]
rsequence = foldr (rliftM2 (:)) $ rreturn []

rmapM :: (LnMonad m, Rfl Link m i, Rfl Init m i) => (a -> m i i b) -> [a] -> m i i [b]
rmapM f = rsequence . map f

rsequence_ :: (LnMonad m, Rfl Link m i, Rfl Init m i) => [m i i a] -> m i i ()
rsequence_ = foldr (>>>) $ rreturn ()

rmapM_ :: (LnMonad m, Rfl Link m i, Rfl Init m i) => (a -> m i i b) -> [a] -> m i i ()
rmapM_ f = rsequence_ . map f

-- }}}

-- Copointed {{{

class (LnFunctor w, LnTerminal w) => LnCopointed (w :: ix -> ix -> * -> *) where
  lextract :: Term w i j => w i j a -> a
  default lextract :: (IxCopointed w, Link w j i, Term w i j)
    => w i j a -> a
  lextract = iextract . weaken

-- }}}

