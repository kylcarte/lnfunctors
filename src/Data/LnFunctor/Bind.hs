{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Bind where

import Data.LnFunctor
import Data.LnFunctor.Apply

import Prelude hiding ((>>=),(>>))

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

