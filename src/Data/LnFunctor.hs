{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor
  ( module Data.LnFunctor
  , IxFunctor(..)
  ) where

import Prelude hiding ((>>=),(>>))
import Data.Proxy
import Type.Families
import GHC.Exts (IsList(..))

-- Class Constraints {{{

type Rfl
  (c :: k -> ix -> ix -> l)
  (f :: k)
  (i :: ix)
  = c f i i

type Rfl2
  (c :: k -> ix -> ix -> ix -> ix -> l)
  (f :: k)
  (i :: ix)
  (j :: ix)
  = c f i j i j

type Rfl4
  (c :: k -> ix -> ix -> ix -> ix -> l)
  (f :: k)
  (i :: ix)
  = c f i i i i

type Trn
  (c :: l -> ix -> ix -> ix -> ix -> m)
  (f :: l)
  (i :: ix)
  (j :: ix)
  (k :: ix)
  = c f i j j k

type Sym
  (c :: k -> ix -> ix -> Constraint)
  (f :: k)
  (i :: ix)
  (j :: ix)
  = ((c f i j, c f j i) :: Constraint)

class LnFunctor f => LnInitial  (f :: ix -> ix -> * -> *) where
  type Init f (i :: ix) (j :: ix) :: Constraint
class LnFunctor f => LnTerminal (f :: ix -> ix -> * -> *) where
  type Term f (i :: ix) (j :: ix) :: Constraint

class LnFunctor f => LnJoin (f :: ix -> ix -> * -> *) where
  type JoinUp   f (i :: ix) (j :: ix) :: Constraint
  type JoinDown f (i :: ix) (j :: ix) :: Constraint
class LnFunctor f => LnSplit (f :: ix -> ix -> * -> *) where
  type SplitUp    f (i :: ix) (j :: ix) :: Constraint
  type SplitDown  f (i :: ix) (j :: ix) :: Constraint

{-
type Links  f i j k l = (LinkPar f i j k l,LinkSeq f i j k l)
type family LinkPar (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (k :: ix) (l :: ix) where
  LinkPar f i j k l = (LinkL f i k,LinkR f j l)
type family LinkSeq (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (k :: ix) (l :: ix) where
  LinkSeq f i j k l = LinkI f j k
-}

-- }}}

-- Functor {{{

-- Index Restricted {{{

class IxFunctor f where
  imap :: (a -> b) -> f i j a -> f i j b

(<$>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<$>) = imap
infixl 4 <$>

(<$) :: IxFunctor f => a -> f i j b -> f i j a
a <$ f = const a <$> f
infixl 4 <$

($>) :: IxFunctor f => f i j b -> a -> f i j a
($>) = flip (<$)
infixl 4 $>

ivoid :: IxFunctor f => f i j a -> f i j ()
ivoid = (() <$)

-- }}}

type LinkPar f i j k l = (L f i k, R f j l)

-- minimum definition: Link*, weaken and strengthen
class IxFunctor f => LnFunctor (f :: ix -> ix -> * -> *) where
  type L f (i :: ix) (k :: ix) :: Constraint
  type R f (j :: ix) (l :: ix) :: Constraint
  weaken     :: L f i k => f i j a -> f k j a
  strengthen :: R f j l => f i j a -> f i l a
  stretch    :: LinkPar f i j k l => f i j a -> f k l a
  stretch = weaken . strengthen 
  lmap  :: LinkPar f i j k l => (a -> b) -> f i j a -> f k l b
  lmap  f = stretch . imap f
  -- lmap constraining right indices to be equal
  lmapL :: L f i j => (a -> b) -> f i x a -> f j x b
  lmapL f = weaken . imap f
  -- lmap constraining left indices to be equal
  lmapR :: R f i j => (a -> b) -> f x i a -> f x j b
  lmapR f = strengthen . imap f

{-
(<$$>) :: (LnFunctor f, Links f i j k l) => (a -> b) -> f i j a -> f k l b
(<$$>) = lmap
infixl 4 <$$>

(<$$) :: (LnFunctor f, Links f i j k l) => a -> f i j b -> f k l a
a <$$ f = const a <$$> f
infixl 4 <$$

($$>) :: (LnFunctor f, Links f i j k l) => f i j b -> a -> f k l a
($$>) = flip (<$$)
infixl 4 $$>

lvoid :: (LnFunctor f, Links f i j k l) => f i j a -> f k l ()
lvoid = (() <$$)
-}

-- }}}

-- Empty {{{

class (LnFunctor f, LnInitial f) => LnEmpty f where
  lempty :: Init f i j => f i j a

iempty :: (LnEmpty f, Rfl Init f i)
  => f i i a
iempty = lempty

-- }}}

-- Pointed {{{

class (LnFunctor m, LnInitial m) => LnPointed (m :: ix -> ix -> * -> *) where
  lreturn :: Init m i j => a -> m i j a

ireturn :: (LnPointed m, Rfl Init m i) => a -> m i i a
ireturn = lreturn

-- }}}

-- Apply {{{

{-
-- Index Restricted {{{

iap :: (LnApply m, Trn Links m i j k) => m i j (a -> b) -> m j k a -> m i k b
iap = lap

(<*>) :: (LnApply m, Trn Links m i j k)
  => m i j (a -> b) -> m j k a -> m i k b
(<*>) = iap
infixl 4 <*>

iliftA :: LnApply m => (a -> b) -> m i j a -> m i j b
iliftA = imap

iliftA2 :: (LnApply m, Trn Links m i j k)
  => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftA2 f a b = f <$> a <*> b

iliftA3 :: (LnApply m, Trn Links m i j k, Trn Links m i k l)
  => (a -> b -> c -> d) -> m i j a -> m j k b -> m k l c -> m i l d
iliftA3 f a b c = f <$> a <*> b <*> c

-- }}}
-}


class LnFunctor f => LnApply (f :: ix -> ix -> * -> *) where
  type Link f (i :: ix) (j :: ix) (k :: ix) (l :: ix)
    (h :: ix) (m :: ix) :: Constraint
  lap :: Link f i j k l h m =>
    f i j (a -> b) -> f k l a -> f h m b

(<**>) :: (LnApply f, Link f i j k l h m)
  => f i j (a -> b) -> f k l a -> f h m b
(<**>) = lap
infixl 4 <**>

{-
lliftA  :: (LnApply m, Links m i j k l) => (a -> b) -> m i j a -> m k l b
lliftA = lmap

lliftA2 :: (LnApply m, Links m i j k l)
  => (a -> b -> c) -> m i j a -> m k l b -> m i l c
lliftA2 f a b = f <$> a <**> b

lliftA3 :: (LnApply m,Links m i j k l,Links m i l n o)
  => (a -> b -> c -> d)
  -> m i j a -> m k l b -> m n o c -> m i o d
lliftA3 f a b c = f <$> a <**> b <**> c

rthenL :: (LnApply m, Links m i j k l)
  => m i j a -> m k l b -> m i l a
rthenL = lliftA2 const

rthenR :: (LnApply m, Links m i j k l)
  => m i j a -> m k l b -> m i l b
rthenR = lliftA2 (const id)

(<**) :: (LnApply m, Links m i j k l)
  => m i j a -> m k l b -> m i l a
(<**) = rthenL
infixl 4 <**

(**>) :: (LnApply m, Links m i j k l)
  => m i j a -> m k l b -> m i l b
(**>) = rthenR
infixl 4 **>
-}

-- }}}

{-
-- Alt {{{

-- Index Restricted {{{

(<|>) :: (LnAlt m, Rfl JoinUp m i) => m i j a -> m i j a -> m i j a
(<|>) = lbranchL

(>|<) :: (LnAlt m, Rfl JoinDown m j) => m i j a -> m i j a -> m i j a
(>|<) = lfunnelL

-- }}}

class (LnFunctor m, LnJoin m) => LnAlt m where
  lbranchL :: JoinUp m i j => m i k a -> m j k a -> m i k a
  lbranchR :: JoinUp m i j => m i k a -> m j k a -> m j k a
  lfunnelL :: JoinDown m j k => m i j a -> m i k a -> m i j a
  lfunnelR :: JoinDown m j k => m i j a -> m i k a -> m i k a

(<<|>) :: (LnAlt m,JoinUp m i j) => m i k a -> m j k a -> m i k a
(<<|>) = lbranchL
infixl 3 <<|>

(<|>>) :: (LnAlt m,JoinUp m i j) => m i k a -> m j k a -> m j k a
(<|>>) = lbranchR
infixl 3 <|>>

(>>|<) :: (LnAlt m, JoinDown m j k) => m i j a -> m i k a -> m i j a
(>>|<) = lfunnelL
infixl 3 >>|<

(>|<<) :: (LnAlt m, JoinDown m j k) => m i j a -> m i k a -> m i k a
(>|<<) = lfunnelR
infixl 3 >|<<

-- }}}
-}

-- Bind {{{

{-
-- Index Restricted {{{

ibind :: (LnBind m, Trn Links m i j k) => m i j a -> (a -> m j k b) -> m i k b
ibind = lbind

ijoin :: (LnBind m, Trn Links m i j k) => m i j (m j k a) -> m i k a
ijoin = ljoin

(>>=) :: (LnBind m, Trn Links m i j k) => m i j a -> (a -> m j k b) -> m i k b
(>>=) = ibind
infixl 1 >>=

(=<<) :: (LnBind m, Trn Links m i j k) => (a -> m j k b) -> m i j a -> m i k b
(=<<) = flip (>>=)
infixr 1 =<<

(>>) :: (LnBind m, Trn Links m i j k) => m i j a -> m j k b -> m i k b
m1 >> m2 = m1 >>= const m2
infixl 1 >>

(>=>) :: (LnBind m, Trn Links m i j k) => (a -> m i j b) -> (b -> m j k c) -> a -> m i k c
(f >=> g) x = f x >>= g
infixr 1 >=>

(<=<) :: (LnBind m, Trn Links m i j k) => (b -> m j k c) -> (a -> m i j b) -> a -> m i k c
(f <=< g) x = g x >>= f
infixr 1 <=<

-- }}}
-}

class LnApply f => LnBind f where
  lbind :: Link f i j k l h m => f i j a -> (a -> f k l b) -> f h m b

{-
ljoin :: (LnBind m, Links m i j k l) => m i j (m k l a) -> m i l a
ljoin = flip lbind id

(>>>=) :: (LnBind m, Links m i j k l) => m i j a -> (a -> m k l b) -> m i l b
(>>>=) = lbind
infixl 1 >>>=

(=<<<) :: (LnBind m, Links m i j k l) => (a -> m k l b) -> m i j a -> m i l b
(=<<<) = flip (>>>=)
infixr 1 =<<<

(>>>) :: (LnBind m, Links m i j k l) => m i j a -> m k l b -> m i l b
m1 >>> m2 = m1 >>>= const m2
infixl 1 >>>

(>>=>) :: (LnBind m, Links m i j k l) => (a -> m i j b) -> (b -> m k l c) -> a -> m i l c
(f >>=> g) x = f x >>>= g
infixr 1 >>=>

(<=<<) :: (LnBind m, Links m i j k l) => (b -> m k l c) -> (a -> m i j b) -> a -> m i l c
(f <=<< g) x = g x >>>= f
infixr 1 <=<<
-}

-- }}}

{-
-- Monad {{{

-- Index Restricted {{{

iliftM :: LnMonad m => (a -> b) -> m i j a -> m i j b
iliftM = iliftA

iliftM2 :: (LnMonad m, Trn Links m i j k) => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 = iliftA2

iliftM3 :: (LnMonad m, Trn Links m i j k, Trn Links m i k l) => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 = iliftA3

isequence :: (LnMonad m, Rfl4 Links m i, Rfl Init m i) => [m i i a] -> m i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: (LnMonad m, Rfl4 Links m i, Rfl Init m i) => (a -> m i i b) -> [a] -> m i i [b]
imapM f = isequence . map f

isequence_ :: (LnMonad m, Rfl4 Links m i, Rfl Init m i) => [m i i a] -> m i i ()
isequence_ = foldr (>>) $ ireturn ()

imapM_ :: (LnMonad m, Rfl4 Links m i, Rfl Init m i) => (a -> m i i b) -> [a] -> m i i ()
imapM_ f = isequence_ . map f

-- }}}

type LnMonad  m = (LnPointed m, LnBind m)

lliftM :: (LnMonad m, Links m i j k l) => (a -> b) -> m i j a -> m k l b
lliftM = lliftA

lliftM2 :: (LnMonad m, Links m i j k l) => (a -> b -> c) -> m i j a -> m k l b -> m i l c
lliftM2 = lliftA2

lliftM3 :: (LnMonad m, Links m i j k l, Links m i l n o) => (a -> b -> c -> d)
  -> m i j a -> m k l b -> m n o c -> m i o d
lliftM3 = lliftA3

{-
tsequence :: (LnMonad m, Init m i lst) => TrList m mt i j lst is a -> m i lst [a]
tsequence (tl :: TrList m mt i j lst is a) = case tl of
  NilL     -> lreturn []
  OneL mij -> imap (:[]) mij
  ConsL (mij :: m i j a) (tl' :: TrList m mt j k lst is' a) -> mij >>= \a -> mas >>= \as -> ireturn (a : as)
    where
    mas :: m j lst [a]
    mas = tsequence tl'

data TrList (m :: ix -> ix -> * -> *) (empty :: Bool) (i :: ix) (j :: ix) (lst :: ix) (is :: [Pr ix ix]) :: * -> * where
  NilL  :: TrList m True i j j '[] a
  OneL  :: (Init m j j, LinkR m j j) => m i j a -> TrList m False i j j '[i:*:j] a
  ConsL :: (Trn Links m i j k, Init m lst lst, Links m i j lst lst, Init m j lst, Links m j lst lst lst)
    => m i j a -> TrList m False j k lst is a -> TrList m False i j lst (i:*:j ': is) a

l0 :: TrList TrivLn True i j j '[] a
l0 = NilL
-}

{-

-- Transitively Linked List {{{

tsequence :: (LnMonad m) => TrList m i j lst is a -> m i lst [a]
tsequence tl = case tl of
  NilL          -> lreturn []
  ConsL mij ls' -> mij >>= \a ->
    tsequence ls' >>= \as ->
    ireturn (a:as)

data TrList (m :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (lst :: ix) (is :: [ix]) :: * -> * where
  NilL  :: TrList m i i i '[i] a
  ConsL :: (Trn Links m i j k, Trn Links m j lst lst, LinkR m j lst) => m i j a -> TrList m j k lst is a -> TrList m i j lst (i ': is) a

l0 :: TrList TrivLn A A A '[A] ()
l0 = NilL

l1 :: TrList TrivLn A B B '[A,B] ()
l1 = ConsL m1 NilL

l2 :: TrList TrivLn A B C '[A,B,C] ()
l2 = ConsL m1 $ ConsL m2 NilL

l3 :: TrList TrivLn A B D '[A,B,C,D] ()
l3 = ConsL m1 $ ConsL m2 $ ConsL m3 NilL

m1 :: TrivLn A B ()
m1 = lreturn ()

m2 :: TrivLn B C ()
m2 = lreturn ()

m3 :: TrivLn C D ()
m3 = lreturn ()

-- }}}

-}

-- }}}

-}

{-

-- Copointed {{{

class (LnFunctor w, LnTerminal w) => LnCopointed (w :: ix -> ix -> * -> *) where
  lextract :: Term w i j => w i j a -> a

-- }}}


-- Old Class Constraints {{{

-- , LinkOuter  f ls
-- , LinkGen    f ls
-- , LinkEnds   f ls

{-
type family LinkBounds f ls where
  LinkBounds f ls = (LinkOuter f ls, LinkEnds f ls)
-}

{-
type family LinkGen (f :: ix -> ix -> * -> *) (ls :: [Pr ix ix]) :: Constraint where
  LinkGen f  (i:*:j ': k:*:l ': ls )
    = ( LinkG   f i j k l
      , LinkGen f (k:*:l ': ls)
      )
  LinkGen f ls = ()
-}

{-
type family LinkOuter (f :: ix -> ix -> * -> *) (ls :: [Pr ix ix]) :: Constraint where
  LinkOuter f  (i:*:j ': k:*:l ': ls )
    = ( LinkO     f i l
      , LinkOuter f (k:*:l ': ls)
      )
  LinkOuter f ls = ()
-}

{-
type family LinkEnds (f :: ix -> ix -> * -> *) (ls :: [Pr ix ix]) :: Constraint where
  LinkEnds f '[]              = ()
  LinkEnds f (i:*:j ': ls ) = LinkEnds_ f i (i:*:j ': ls )
type family LinkEnds_ (f :: ix -> ix -> * -> *) (i :: ix) (ls :: [Pr ix ix]) :: Constraint where
  LinkEnds_ f i '[j:*:k ]           = LinkE f i k
  LinkEnds_ f i ( p ': l:*:m ': ls ) = LinkEnds_ f i (l:*:m ': ls)
  LinkEnds_ f i ls                    = ()
-}

-- }}}


-- Test {{{

data TrivLn (i :: Index) (j :: Index) a = Trv
  { runTrivLn :: a
  } deriving (Eq,Show)

data Index = A | B | C | D | E | F deriving (Eq,Show)

instance IxFunctor TrivLn where
  imap f (Trv a) = Trv $ f a

instance LnFunctor TrivLn where
  type LinkL TrivLn i j = TrivL i j
  type LinkR TrivLn i j = TrivR i j
  type LinkI TrivLn i j = TrivI i j
  weaken (Trv a) = Trv a
  strengthen (Trv a) = Trv a

instance LnInitial TrivLn where
  type Init TrivLn i j = ()

instance LnPointed TrivLn where
  lreturn a = Trv a

instance LnApply TrivLn where
  lap (Trv f) (Trv a) = Trv $ f a

instance LnBind TrivLn where
  lbind (Trv a) f = case f a of
    Trv a -> Trv a

data Tag
  = TLeft
  | TRight
  | TInner
  deriving (Eq,Show)

class TrivL (i :: Index) (j :: Index) where
  trivL :: Proxy (TLeft:*:i:*:j)
  trivL = Proxy

class TrivR (i :: Index) (j :: Index) where
  trivR :: Proxy (TRight:*:i:*:j)
  trivR = Proxy

class TrivI (i :: Index) (j :: Index) where
  trivI :: Proxy (TInner:*:i:*:j)
  trivI = Proxy

{-
class TrivInit (i :: Index) (j :: Index) where
  trivInit :: Proxy (TInit:*:i:*:j)
  trivInit = Proxy
-}

instance TrivL i j
instance TrivR i j
instance TrivI i j

-- }}}

-}
