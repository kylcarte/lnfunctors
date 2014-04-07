{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Apply where

import Data.LnFunctor
import Type.Families

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

