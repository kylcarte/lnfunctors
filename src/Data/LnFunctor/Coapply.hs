{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Coapply where

import Data.LnFunctor

class LnFunctor f => LnCoapply f where
  type Unlink f h m k l i j :: Constraint
  lcoap :: Unlink f h m k l i j => f h m (a -> b) -> f k l a -> f i j b

(<@@>) :: (LnCoapply f, Unlink f h m k l i j)
  => f h m (a -> b) -> f k l a -> f i j b
(<@@>) = lcoap
infixl 4 <@@>

(<@@) :: (LnCoapply f, Unlink f h m k l i j) => f h m a -> f k l b -> f i j a
(<@@) = lcothenL
infixl 4 <@@

(@@>) :: (LnCoapply f, Unlink f h m k l i j) => f h m a -> f k l b -> f i j b
(@@>) = lcothenR
infixl 4 @@>

lcothenL :: (LnCoapply f, Unlink f h m k l i j) => f h m a -> f k l b -> f i j a
lcothenL = lliftCA2 const

lcothenR :: (LnCoapply f, Unlink f h m k l i j) => f h m a -> f k l b -> f i j b
lcothenR = lliftCA2 (const id)

lliftCA :: (LnCoapply f,LinkPar f i j k l) => (a -> b) -> f i j a -> f k l b
lliftCA = (<$$>)

lliftCA2  :: (LnCoapply f, Unlink f h m k l i j) => (a -> b -> c) -> f h m a -> f k l b -> f i j c
lliftCA2 f a b = f <$> a <@@> b

lliftCA3 :: forall f a b c d h i j k l m n o p q.
  (LnCoapply f, Unlink f h m k l i j, Unlink f p q n o h m)
  => (a -> b -> c -> d)
  -> f p q a -> f n o b -> f k l c -> f i j d
lliftCA3 f a b c = fab <@@> c
  where
  fa :: f p q (b -> c -> d)
  fa = f <$> a
  fab :: f h m (c -> d)
  fab = fa <@@> b

