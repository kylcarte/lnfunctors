{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Coalt where

import Data.LnFunctor

-- XXX : types are unfinished, they are pasted from Alt

class LnFunctor f => LnSplit (f :: ix -> ix -> * -> *) where
  type SplitUp    f (i :: ix) (j :: ix) (k :: ix) :: Constraint
  type SplitDown  f (i :: ix) (j :: ix) (k :: ix) :: Constraint

class (LnFunctor f, LnSplit f) => LnCoalt f where
  lsplit   :: SplitUp   f i j k => f i l a -> f j l a -> f k l a
  lexplode :: SplitDown f j k l => f i j a -> f i k a -> f i l a

(<^>) :: (LnCoalt f,SplitUp f i j k) => f i l a -> f j l a -> f k l a
(<^>) = lsplit
infixl 3 <|>

(>^<) :: (LnCoalt f, SplitDown f j k l) => f i j a -> f i k a -> f i l a
(>^<) = lexplode
infixl 3 >|<

-- Index Restricted {{{

type ISplitUp   f i = SplitUp   f i i i
type ISplitDown f i = SplitDown f i i i

ibranch :: (LnCoalt f, ISplitUp f i) => f i j a -> f i j a -> f i j a
ibranch = lbranch

ifunnel :: (LnCoalt f, ISplitDown f j) => f i j a -> f i j a -> f i j a
ifunnel = lfunnel

-- }}}

