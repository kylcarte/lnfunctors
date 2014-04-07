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
  type JoinUp   f (i :: ix) (j :: ix) (k :: ix) :: Constraint
  type JoinDown f (i :: ix) (j :: ix) (k :: ix) :: Constraint
class LnFunctor f => LnSplit (f :: ix -> ix -> * -> *) where
  type SplitUp    f (i :: ix) (j :: ix) (k :: ix) :: Constraint
  type SplitDown  f (i :: ix) (j :: ix) (k :: ix) :: Constraint

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

(<$$>) :: (LnFunctor f, LinkPar f i j k l) => (a -> b) -> f i j a -> f k l b
(<$$>) = lmap
infixl 4 <$$>

(<$$) :: (LnFunctor f, LinkPar f i j k l) => a -> f i j b -> f k l a
a <$$ f = const a <$$> f
infixl 4 <$$

($$>) :: (LnFunctor f, LinkPar f i j k l) => f i j b -> a -> f k l a
($$>) = flip (<$$)
infixl 4 $$>

lvoid :: (LnFunctor f, LinkPar f i j k l) => f i j a -> f k l ()
lvoid = (() <$$)

-- }}}

-- Empty {{{

class (LnFunctor f, LnInitial f) => LnEmpty f where
  lempty :: Init f i j => f i j a

iempty :: (LnEmpty f, Rfl Init f i)
  => f i i a
iempty = lempty

-- }}}

-- Pointed {{{

class (LnFunctor f, LnInitial f) => LnPointed (f :: ix -> ix -> * -> *) where
  lreturn :: Init f i j => a -> f i j a

ireturn :: (LnPointed f, Rfl Init f i) => a -> f i i a
ireturn = lreturn

-- }}}

-- Apply {{{

-- Index Restricted {{{

type ILink f i j k = Link f i j j k i k

iap :: (LnApply f, ILink f i j k) => f i j (a -> b) -> f j k a -> f i k b
iap = lap

(<*>) :: (LnApply f, ILink f i j k)
  => f i j (a -> b) -> f j k a -> f i k b
(<*>) = iap
infixl 4 <*>

iliftA :: LnApply f => (a -> b) -> f i j a -> f i j b
iliftA = imap

iliftA2 :: (LnApply f, ILink f i j k)
  => (a -> b -> c) -> f i j a -> f j k b -> f i k c
iliftA2 f a b = f <$> a <*> b

iliftA3 :: (LnApply f, ILink f i j k, ILink f i k l)
  => (a -> b -> c -> d) -> f i j a -> f j k b -> f k l c -> f i l d
iliftA3 f a b c = f <$> a <*> b <*> c

-- }}}


class LnFunctor f => LnApply (f :: ix -> ix -> * -> *) where
  type Link f (i :: ix) (j :: ix) (k :: ix) (l :: ix)
    (h :: ix) (m :: ix) :: Constraint
  lap :: Link f i j k l h m =>
    f i j (a -> b) -> f k l a -> f h m b

(<**>) :: (LnApply f, Link f i j k l h m)
  => f i j (a -> b) -> f k l a -> f h m b
(<**>) = lap
infixl 4 <**>

lliftA  :: (LnApply f, LinkPar f i j k l) => (a -> b) -> f i j a -> f k l b
lliftA = lmap

lliftA2 :: (LnApply f, Link f i j k l h m)
  => (a -> b -> c) -> f i j a -> f k l b -> f h m c
lliftA2 f a b = f <$> a <**> b

lliftA3 :: forall f a b c d h i j k l m n o p q.
  (LnApply f,Link f i j k l h m,Link f h m n o p q)
  => (a -> b -> c -> d)
  -> f i j a -> f k l b -> f n o c -> f p q d
lliftA3 f a b c = fab <**> c
  where
  fa :: f i j (b -> c -> d)
  fa = f <$> a
  fab :: f h m (c -> d)
  fab = fa <**> b

lthenL :: (LnApply f, Link f i j k l h m)
  => f i j a -> f k l b -> f h m a
lthenL = lliftA2 const

lthenR :: (LnApply f, Link f i j k l h m)
  => f i j a -> f k l b -> f h m b
lthenR = lliftA2 (const id)

(<**) :: (LnApply f, Link f i j k l h m)
  => f i j a -> f k l b -> f h m a
(<**) = lthenL
infixl 4 <**

(**>) :: (LnApply f, Link f i j k l h m)
  => f i j a -> f k l b -> f h m b
(**>) = lthenR
infixl 4 **>

-- }}}

-- Alt {{{

-- Index Restricted {{{

type IJoinUp   f i = JoinUp   f i i i
type IJoinDown f i = JoinDown f i i i

ibranch :: (LnAlt f, IJoinUp f i) => f i j a -> f i j a -> f i j a
ibranch = lbranch

ifunnel :: (LnAlt f, IJoinDown f j) => f i j a -> f i j a -> f i j a
ifunnel = lfunnel

-- }}}

class (LnFunctor f, LnJoin f) => LnAlt f where
  lbranch :: JoinUp   f i j k => f i l a -> f j l a -> f k l a
  lfunnel :: JoinDown f j k l => f i j a -> f i k a -> f i l a

(<|>) :: (LnAlt f,JoinUp f i j k) => f i l a -> f j l a -> f k l a
(<|>) = lbranch
infixl 3 <|>

(>|<) :: (LnAlt f, JoinDown f j k l) => f i j a -> f i k a -> f i l a
(>|<) = lfunnel
infixl 3 >|<

-- }}}

-- Bind {{{

-- Index Restricted {{{

ibind :: (LnBind f, ILink f i j k) => f i j a -> (a -> f j k b) -> f i k b
ibind = lbind

ijoin :: (LnBind f, ILink f i j k) => f i j (f j k a) -> f i k a
ijoin = ljoin

(>>=) :: (LnBind f, ILink f i j k) => f i j a -> (a -> f j k b) -> f i k b
(>>=) = ibind
infixl 1 >>=

(=<<) :: (LnBind f, ILink f i j k) => (a -> f j k b) -> f i j a -> f i k b
(=<<) = flip (>>=)
infixr 1 =<<

(>>) :: (LnBind f, ILink f i j k) => f i j a -> f j k b -> f i k b
m1 >> m2 = m1 >>= const m2
infixl 1 >>

(>=>) :: (LnBind f, ILink f i j k) => (a -> f i j b) -> (b -> f j k c) -> a -> f i k c
(f >=> g) x = f x >>= g
infixr 1 >=>

(<=<) :: (LnBind f, ILink f i j k) => (b -> f j k c) -> (a -> f i j b) -> a -> f i k c
(f <=< g) x = g x >>= f
infixr 1 <=<

-- }}}

class LnApply f => LnBind f where
  lbind :: Link f i j k l h m => f i j a -> (a -> f k l b) -> f h m b

ljoin :: (LnBind f, Link f i j k l h m) => f i j (f k l a) -> f h m a
ljoin = flip lbind id

(>>>=) :: (LnBind f, Link f i j k l h m) => f i j a -> (a -> f k l b) -> f h m b
(>>>=) = lbind
infixl 1 >>>=

(=<<<) :: (LnBind f, Link f i j k l h m) => (a -> f k l b) -> f i j a -> f h m b
(=<<<) = flip (>>>=)
infixr 1 =<<<

(>>>) :: (LnBind f, Link f i j k l h m) => f i j a -> f k l b -> f h m b
m1 >>> m2 = m1 >>>= const m2
infixl 1 >>>

(>>=>) :: (LnBind f, Link f i j k l h m) => (a -> f i j b) -> (b -> f k l c) -> a -> f h m c
(f >>=> g) x = f x >>>= g
infixr 1 >>=>

(<=<<) :: (LnBind f, Link f i j k l h m) => (b -> f k l c) -> (a -> f i j b) -> a -> f h m c
(f <=<< g) x = g x >>>= f
infixr 1 <=<<

-- }}}

-- Monad {{{

{-
-- Index Restricted {{{

iliftM :: LnMonad f => (a -> b) -> f i j a -> f i j b
iliftM = iliftA

iliftM2 :: (LnMonad f, Trn Links f i j k) => (a -> b -> c) -> f i j a -> f j k b -> f i k c
iliftM2 = iliftA2

iliftM3 :: (LnMonad f, Trn Links f i j k, Trn Links f i k l) => (a -> b -> c -> d)
  -> f i j a -> f j k b -> f k l c -> f i l d
iliftM3 = iliftA3

isequence :: (LnMonad f, Rfl4 Links f i, Rfl Init f i) => [f i i a] -> f i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: (LnMonad f, Rfl4 Links f i, Rfl Init f i) => (a -> f i i b) -> [a] -> f i i [b]
imapM f = isequence . map f

isequence_ :: (LnMonad f, Rfl4 Links f i, Rfl Init f i) => [f i i a] -> f i i ()
isequence_ = foldr (>>) $ ireturn ()

imapM_ :: (LnMonad f, Rfl4 Links f i, Rfl Init f i) => (a -> f i i b) -> [a] -> f i i ()
imapM_ f = isequence_ . map f

-- }}}
-}

type LnMonad  f = (LnPointed f, LnBind f)

{-

-- Transitively Linked List {{{

tsequence :: (LnMonad f) => TrList f i j lst is a -> f i lst [a]
tsequence tl = case tl of
  NilL          -> lreturn []
  ConsL mij ls' -> mij >>= \a ->
    tsequence ls' >>= \as ->
    ireturn (a:as)

data TrList (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (lst :: ix) (is :: [ix]) :: * -> * where
  NilL  :: TrList f i i i '[i] a
  ConsL :: (Trn Links f i j k, Trn Links f j lst lst, LinkR f j lst) => f i j a -> TrList f j k lst is a -> TrList f i j lst (i ': is) a

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


-- Copointed {{{

class (LnFunctor w, LnTerminal w) => LnCopointed (w :: ix -> ix -> * -> *) where
  lextract :: Term w i j => w i j a -> a

-- }}}


{-

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
