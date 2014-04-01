{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Apply where

import Data.LnFunctor

class LnFunctor m => LnApply m where
  lap :: WithLink m j k (m i j (a -> b) -> m k l a -> m i l b)

lthenL :: LnApply m => WithLink m j k (m i j a -> m k l b -> m i l a)
lthenL ljk = lliftA2 ljk const

lthenR :: LnApply m => WithLink m j k (m i j a -> m k l b -> m i l b)
lthenR ljk = lliftA2 ljk (const id)

lliftA :: LnApply m => (a -> b) -> m i j a -> m i j b
lliftA = (<$$>)

lliftA2 :: LnApply m => WithLink m j k
  ((a -> b -> c) -> m i j a -> m k l b -> m i l c)
lliftA2 ljk f = lap ljk . imap f

lliftA3 :: LnApply f => WithLinks f '[ '(j,k) , '(l,m) ]
  ((a -> b -> c -> d) -> f i j a -> f k l b -> f m n c -> f i n d)
lliftA3 ljk llm f a = lap llm . lap ljk (imap f a)

