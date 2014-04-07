{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Alt where

import Data.LnFunctor

class LnFunctor f => LnJoin (f :: ix -> ix -> * -> *) where
  type JoinUp   f (i :: ix) (j :: ix) (k :: ix) :: Constraint
  type JoinDown f (i :: ix) (j :: ix) (k :: ix) :: Constraint

class (LnFunctor f, LnJoin f) => LnAlt f where
  lbundle :: JoinUp   f i j k => f i l a -> f j l a -> f k l a
  lfunnel :: JoinDown f j k l => f i j a -> f i k a -> f i l a

(<|>) :: (LnAlt f,JoinUp f i j k) => f i l a -> f j l a -> f k l a
(<|>) = lbundle
infixl 3 <|>

(>|<) :: (LnAlt f, JoinDown f j k l) => f i j a -> f i k a -> f i l a
(>|<) = lfunnel
infixl 3 >|<

-- Index Restricted {{{

type IJoinUp   f i = JoinUp   f i i i
type IJoinDown f i = JoinDown f i i i

ibundle :: (LnAlt f, IJoinUp f i) => f i j a -> f i j a -> f i j a
ibundle = lbundle

ifunnel :: (LnAlt f, IJoinDown f j) => f i j a -> f i j a -> f i j a
ifunnel = lfunnel

-- }}}

