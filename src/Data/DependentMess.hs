{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.LnFunctor
  ( module Data.LnFunctor
  , module Data.IxFunctor
  , Constraint
  ) where

import Data.IxFunctor
import Data.Proxy
import Data.Tagged
import Data.Reflection hiding (Z,D,SD,PD)

import GHC.Exts (Constraint)

data family Link (f :: ix -> ix -> * -> *) :: ix -> ix -> *

data LinkTag (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) = Link

-- Linked {{{

data Linked s (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) a = Linked
  { runLinked :: f i j a
  } deriving (Eq,Show)

{-
noLinks :: (forall (s :: k). Reifies s (LinkMap f '[]) => Proxy s -> Linked s f i j a) -> f i j a
noLinks lm = reify NilM $ \ps -> runLinked $ lm ps
-}

{-
instance Reifies (LinksTag f '[]) (LinkMap f '[]) where
  reflect _ = NilM
-}

-- }}}

-- LinkList {{{

data N
  = Z
  | S N

data LinkList (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (x :: N) where
  NilL  :: LinkList f i j Z
  ConsL :: Link f i j -> LinkList f i j x -> LinkList f i j (S x)

-- }}}

-- LinkMap {{{

data LinkMap (f :: ix -> ix -> * -> *) (ls :: [((ix,ix),N)]) where
  -- NilM  :: LinkMap f '[]
  ConsM :: LinkList f i j n -> LinkMap f ls -> LinkMap f ( '( '(i,j) , n) ': ls )

type family InsertMap (i :: ix) (j :: ix) (ls :: [((ix,ix),N)]) :: [((ix,ix),N)] where
  InsertMap i j '[]                      = '[ '( '(i,j) , S Z ) ]
  InsertMap i j ( '( '(i,j) , x ) ': ls) = ( '( '(i,j) , S x ) ': ls )
  InsertMap i j ( '( '(k,l) , x ) ': ls) = ( '( '(k,l) ,   x ) ': InsertMap i j ls )

intoMap :: Link f i j -> LinkMap f ls -> LinkMap f (InsertMap i j ls)
intoMap = undefined

-- }}}

-- LMap {{{

emptyLMap :: LMap f '[]
emptyLMap = NilM

type family AddNew (b :: Bool) (n :: N) :: N where
  AddNew False n = S Z
  AddNew True  n = S n

insertLMap :: forall f i j x found ls ls'.
  HasLinks found f i j x ls ls'
  => Link f i j -> LMap f ls
  -> LMap f ( '( '(i,j), LinkList f i j (AddNew found x)) ': ls')

insertLMap l lm = case v of
  (No,lm') -> InsM t (ConsL l NilL) lm'
  (Yes ll,lm') -> InsM t (ConsL l ll) lm'
  where
  t = Link :: LinkTag f i j
  v = view t lm :: (Exist found (LinkList f i j x),LMap f ls')

lookupLMap :: forall found f i j x ls ls'.
  HasLinks found f i j x ls ls'
  => LinkTag f i j -> LMap f ls -> Exist found (LinkList f i j x)
lookupLMap t lm = case (view t lm :: (Exist found (LinkList f i j x), LMap f ls')) of
  (e,_) -> e

-- }}}

-- View {{{

data LMap (f :: ix -> ix -> * -> *) (ls :: [((ix,ix),*)]) where
  NilM  :: LMap f '[]
  InsM :: LinkTag f i j -> LinkList f i j x -> LMap f ls -> LMap f ( '( '(i,j),LinkList f i j x) ': ls )

view :: forall found f i j ls ls' a x. HasLinks found f i j x ls ls' => LinkTag f i j -> LMap f ls -> (Exist found (LinkList f i j x),LMap f ls')
view a = go . getView a
  where
  go :: View found_ f_ i_ j_ x_ ls_ -> (Exist found_ (LinkList f_ i_ j_ x_),LMap f_ ls_)
  go v = case v of
    NoView        -> (No,NilM)
    Found ll ls  -> (Yes ll,ls)
    Later t l v'  -> fmap (InsM t l) $ go v'

data View (found :: Bool) (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (x :: N) (ls :: [((ix,ix),*)]) where
  NoView :: View False f i j x '[]
  Found  :: LinkList f i j x -> LMap f ls -> View True f i j x ls
  Later  :: LinkTag f k l -> LinkList f k l x -> View found f i j x' ls -> View found f i j x' ( '( '(k,l),LinkList f k l x) ': ls )

class HasLinks (found :: Bool) (f :: ix -> ix -> * -> *) (i :: ix) (j :: ix) (x :: N) (ls :: [((ix,ix),*)]) (ls' :: [((ix,ix),*)]) where
  getView :: LinkTag f i j -> LMap f ls -> View found f i j x ls'

instance HasLinks False f i j x '[] '[] where
  getView _ NilM = NoView

instance HasLinks True f i j x ( '( '(i,j) , LinkList f i j x) ': ls ) ls where
  getView a (InsM t l ls) = Found l ls

instance HasLinks found f i j x ls ls'
  => HasLinks found f i j x ( '( '(k,l),ll) ': ls ) ( '( '(k,l),ll) ': ls' ) where
  getView a (InsM t l ls) = Later t l $ getView a ls

data Exist :: Bool -> * -> * where
  No  :: Exist False a
  Yes :: a -> Exist True a

-- }}}

{-
-- LMap {{{

type LMap = Assocs

emptyLMap :: LMap '[]
emptyLMap = NilA

type family AddNew (b :: Bool) (n :: N) :: N where
  AddNew False n = S Z
  AddNew True  n = S n

insertLMap :: forall f i j x found tls tls'.
  HasView (LinkTag f i j) (LinkList f i j x) found tls tls'
  => Link f i j -> LMap tls
  -> LMap ( '(LinkTag f i j, LinkList f i j (AddNew found x)) ': tls')

insertLMap l lm = case v of
  (No,lm') -> ConsA (t,ConsV l NilV) lm'
  (Yes ll,lm') -> ConsA (t,ConsV l ll) lm'
  where
  t = Link :: LinkTag f i j
  v = view t lm :: (Exist found (LinkList f i j x),LMap tls')

lookupLMap :: forall f i j x found tls tls'.
  HasView (LinkTag f i j) (LinkList f i j x) found tls tls'
  => LinkTag f i j -> LMap tls -> Exist found (LinkList f i j x)
lookupLMap t lm = fst $ view t lm

-- }}}

-- View {{{

data Assocs (abs :: [(*,*)]) where
  NilA  :: Assocs '[]
  ConsA :: (a,b) -> Assocs abs -> Assocs ( '(a,b) ': abs )

view :: HasView a b f abs abs' => a -> Assocs abs -> (Exist f b,Assocs abs')
view a = go . getView a
  where
  go :: View g c d cds -> (Exist g d,Assocs cds)
  go v = case v of
    NoView      -> (No,NilA)
    Found b abs -> (Yes b,abs)
    Later p v'  -> fmap (ConsA p) $ go v'

data View (found :: Bool) (a :: *) (b :: *) (abs :: [(*,*)]) where
  NoView :: View False a b '[]
  Found  :: b -> Assocs abs -> View True a b abs
  Later  :: (c,d) -> View f a b abs -> View f a b ( '(c,d) ': abs )

class HasView (a :: *) (b :: *) (f :: Bool) (abs :: [(*,*)]) (abs' :: [(*,*)]) where
  getView :: a -> Assocs abs -> View f a b abs'

instance HasView a b False '[] '[] where
  getView _ NilA = NoView

instance HasView a b True ( '(a,b) ': abs ) abs where
  getView a (ConsA p abs) = Found (snd p) abs

instance HasView a b f abs abs'
  => HasView a b f ( '(c,d) ': abs ) ( '(c,d) ': abs' ) where
  getView a (ConsA p abs) = Later p $ getView a abs

data Exist :: Bool -> * -> * where
  No  :: Exist False a
  Yes :: a -> Exist True a

-- }}}
-}

