{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor
  ( module Data.LnFunctor
  , Constraint
  ) where

import GHC.Exts (Constraint)

-- | This class provides a way to map under an indexed functor of kind 'k -> k -> * -> *'
-- without altering the indices.
class IxFunctor f where
  imap :: (a -> b) -> f i j a -> f i j b

-- | An LnFunctor is an IxFunctor that knows how to stretch either of its indices.
-- The associated types 'L' and 'R' dictate the conditions under which the
-- initial or terminal index of the functor may be altered, respectively.
-- 
-- Minimum definition: 'L', 'R', 'weaken', and 'strengthen'
class IxFunctor f => LnFunctor (f :: ix -> ix -> * -> *) where
  -- | The constraint (or, assisting class) for stretching the initial index, by 'weaken'
  type L f (i :: ix) (k :: ix) :: Constraint
  -- | The constraint (or, assisting class) for stretching the terminal index, by 'strengthen'
  type R f (j :: ix) (l :: ix) :: Constraint
  weaken     :: L f i k => f i j a -> f k j a
  strengthen :: R f j l => f i j a -> f i l a
  -- | stretch both indices with their respective constraints
  stretch    :: LinkPar f i j k l => f i j a -> f k l a
  stretch = weaken . strengthen 
  -- | map and stretch simultaneously
  lmap  :: LinkPar f i j k l => (a -> b) -> f i j a -> f k l b
  lmap  f = stretch . imap f
  -- | lmap stretching only left index
  lmapL :: L f i j => (a -> b) -> f i x a -> f j x b
  lmapL f = weaken . imap f
  -- | lmap stretching only right index
  lmapR :: R f i j => (a -> b) -> f x i a -> f x j b
  lmapR f = strengthen . imap f

-- | LinkPar constrains the initial and terminal parameters.
-- @
-- 'LinkPar' f i j k l = ('L' f i k, 'R' f j l)
-- @
type LinkPar f i j k l = (L f i k, R f j l)

-- | An infix operator for 'lmap'
(<$$>) :: (LnFunctor f, LinkPar f i j k l) => (a -> b) -> f i j a -> f k l b
(<$$>) = lmap
infixl 4 <$$>

-- | An infix operator analogous to '<$', stretching the functor's indices
(<$$) :: (LnFunctor f, LinkPar f i j k l) => a -> f i j b -> f k l a
a <$$ f = const a <$$> f
infixl 4 <$$

-- | An operator analogous to '$>', stretching the functor's indices
($$>) :: (LnFunctor f, LinkPar f i j k l) => f i j b -> a -> f k l a
($$>) = flip (<$$)
infixl 4 $$>

-- | stretches the functor's indices and replaces the value under the functor with '()'
lvoid :: (LnFunctor f, LinkPar f i j k l) => f i j a -> f k l ()
lvoid = (() <$$)

--------------------------------------------------------------------------------
-- Index Restricted LnFunctor Operations {{{
--------------------------------------------------------------------------------

(<$>) :: IxFunctor f => (a -> b) -> f i j a -> f i j b
(<$>) = imap
infixl 4 <$>

(<$) :: IxFunctor f => a -> f i j b -> f i j a
a <$ f = const a <$> f
infixl 4 <$

($>) :: IxFunctor f => f i j b -> a -> f i j a
($>) = flip (<$)
infixl 4 $>

void :: IxFunctor f => f i j a -> f i j ()
void = (() <$)

-- }}}
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Initial and Terminal Indices {{{
--------------------------------------------------------------------------------

-- | This class specifies the constraint for indexing new functoral values,
-- such as from 'Pointed' or 'Empty'
class LnFunctor f => LnInitial (f :: ix -> ix -> * -> *) where
  type Init f (i :: ix) (j :: ix) :: Constraint

-- | This class specifies the constraint for indexing final functor values,
-- such as with 'lextract' from 'Copointed'
class LnFunctor f => LnTerminal (f :: ix -> ix -> * -> *) where
  type Term f (i :: ix) (j :: ix) :: Constraint

-- }}}
--------------------------------------------------------------------------------

