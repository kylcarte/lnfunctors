{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Type.Families
  ( module Type.Families
  , Constraint
  ) where

import GHC.Exts (Constraint,IsList(..))
import Data.Proxy

-- Free {{{

-- Imposes no constrictions, provides no assistance
class Free2 (i :: ix) (j :: ix)
instance Free2 i j

-- Imposes no constrictions, provides no assistance
class Free4 (i :: ix) (j :: ix) (k :: ix) (l :: ix)
instance Free4 i j k l

-- }}}

-- (Un)Curry {{{

type family Curry (f :: Pr k l -> m) (a :: k) (b :: l) :: m where
  Curry f a b = f (a:*:b)

type family Uncurry (f :: k -> l -> m) (p :: Pr k l) :: m where
  Uncurry f (a:*:b) = f a b

type family O (f :: l -> m) (g :: k -> l) (x :: k) :: m where
  O f g x = f (g x)

-- }}}

-- Products {{{

-- More convenient syntax for promoted tuple
data Pr a b = a :*: b
infixr 6 :*:

type family Fst (p :: Pr k l) :: k where
  Fst (a:*:b) = a

type family Snd (p :: Pr k l) :: l where
  Snd (a:*:b) = b

type family First (f :: k -> m) (p :: Pr k l) :: Pr m l where
  First f (a:*:b) = f a :*: b

type family Second (f :: k -> m) (p :: Pr l k) :: Pr l m where
  Second f (a:*:b) = a :*: f b

-- }}}

-- Lists {{{

type family as :++ bs where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)

type family Concat (ls :: [[k]]) :: [k] where
  Concat '[]       = '[]
  Concat (l ': ls) = l :++ Concat ls

type family Map (f :: k -> l) (as :: [k]) :: [l] where
  Map f '[]       = '[]
  Map f (a ': as) = f a ': Map f as

type family FoldlC (f :: k -> l -> Pr k Constraint) (b :: Pr k Constraint) (as :: [l]) :: Pr k Constraint where
  FoldlC f bc '[]            = bc
  FoldlC f (b:*:c) (a ': as) = FoldlC f (AddC c (f b a)) as

type family Foldl (f :: k -> l -> k) (b :: k) (as :: [l]) :: k where
  Foldl f b '[]       = b
  Foldl f b (a ': as) = Foldl f (f b a) as

type family FoldrC (f :: l -> k -> Pr k Constraint) (b :: Pr k Constraint) (as :: [l]) :: Pr k Constraint where
  FoldrC f bc '[]       = bc
  FoldrC f bc (a ': as) = BindC (FoldrC f bc as) (f a)

type family Foldr (f :: l -> k -> k) (b :: k) (as :: [l]) :: k where
  Foldr f b '[]       = b
  Foldr f b (a ': as) = f a (Foldr f b as)

-- }}}

-- Constraints {{{

type EmptyC      = (() :: Constraint)
type JoinC c1 c2 = ((c1,c2) :: Constraint)

type family MergeC (cs :: [Constraint]) :: Constraint where
  MergeC '[]       = ()
  MergeC (c ': cs) = (c,MergeC cs)

type family AddC (c :: Constraint) (b :: Pr k Constraint) :: Pr k Constraint where
  AddC c1 (b:*:c2) = b:*:((c1,c2) :: Constraint)

type family BindC (bc :: Pr k Constraint) (f :: k -> Pr k Constraint) :: Pr k Constraint where
  BindC (b:*:c) f = AddC c (f b)

type family ExtractC (bc :: Pr k Constraint) :: Constraint where
  ExtractC (b:*:c) = c

-- }}}

data Kind a = K a
type ConstraintK = K (()  :: Constraint)
type ProductK    = K (:*:)
type ListK       = K '[]

type family (f :: Kind (k -> l)) :@ (a :: Kind k) :: Kind l where
  K f :@ K a = K (f a)
infixl 4 :@

type family UnK (m :: Kind k) :: k where
  UnK (K a) = a

type Flip f b a = f a b

-- Monoid {{{

class KindMonoid (m :: Kind k) where
  type Mempty m :: k
  type MappendOf m (a :: k) (b :: k) :: k

type Mappend a b = MappendOf (K a) a b

instance KindMonoid ConstraintK where
  type Mempty ConstraintK = EmptyC
  type MappendOf ConstraintK c1 c2 = JoinC c1 c2

instance KindMonoid ListK where
  type Mempty ListK    = '[]
  type MappendOf ListK l1 l2 = l1 :++ l2

class KindRing (rk :: Kind r) where
  type RA1   rk :: r
  type RAdd  rk (a :: r) (b :: r) :: r
  type RM1   rk :: r
  type RMul  rk (a :: r) (b :: r) :: r

class KindGroup (gk :: Kind g) where
  type GAdd  gk (a :: g) (b :: g) :: g
  type GInv  gk (a :: g)          :: g

class (KindRing rk, KindGroup gk) =>
  KindRModule (rk :: Kind r) (gk :: Kind g) where
  type RMMul rk gk (a :: r) (b :: g) :: g

-- }}}}

-- Monad {{{

class KindMonad (m :: Kind (k -> l) -> *) where
  type Return m (a :: k)               :: l
  type Bind   m (a :: l) (f :: k -> l) :: l

data WK w m where
  WriterK :: WK w (ProductK :@ w)

instance KindMonad (WK (x :: Kind w) :: Kind (k -> Pr w k) -> *) where
  type Return (WK x) (a :: k) = Mempty x:*:a
  type Bind   (WK x) (c:*:a :: Pr w k)
                     (f     :: k -> Pr w k)
    = LAppend c (f a)

type family LAppend (a :: w) (b :: Pr w k) :: Pr w k where
  LAppend w1 (w2:*:a) = Mappend w1 w2 :*: a

-- }}}

-- Type Injection {{{

type family a :~ b :: Bool where
  a :~ a = True
  a :~ b = False

tEq :: Proxy a -> Proxy b -> Proxy (a :~ b)
tEq Proxy Proxy = Proxy

type TEq a b = (EqImpl a b b a, (a :~ b) ~ True )
type TNe a b = (EqImpl a b b a, (a :~ b) ~ False)

data EqProof (e :: Bool) a b where
  Same  :: EqProof True a a
  Diff :: TNe a b => EqProof False a b

class ((b :~ a) ~ e, (a :~ b) ~ e) => EqTest e a b where
  eqProof :: Proxy a -> Proxy b -> EqProof (a :~ b) a b

instance EqTest True a a where
  eqProof Proxy Proxy = Same

instance TNe a b => EqTest False a b where
  eqProof Proxy Proxy = Diff

type EqImpl i j a b = (i :~ j) ~ (a :~ b)

type Inject c i j a b =
  ( EqImpl i j a b             -- equality of indices implies equality of types
  , EqTest (i :~ j) i j        -- we can test equality of indices
  , EqTest (a :~ b) a b        -- we can test equality of data
  , Inject_ c (i :~ j) i j a b -- we can transform data
  )

type family Inject_ c e i j a b:: Constraint where
  Inject_ c True  i j a b = ()
  Inject_ c False i j a b = (InjectNe c i j a b,c i j)

-- dynamic transformation between types
--   if types are equal, use 'id'
--   if not, defer to 'InjectNe' constraint
inject :: forall c i j a b. Inject c i j a b
  => Proxy c -> Proxy i -> Proxy j -> a -> b
inject pc pi pj = case eqProof pa pb of
  Same -> id
  Diff -> inject_ pc
  where
  pa = Proxy :: Proxy a
  pb = Proxy :: Proxy b

class TNe a b => InjectNe c i j a b | c a -> i, c b -> j where
  inject_ :: c i j => Proxy c -> a -> b

-- }}}

class IxMonoid m where
  imempty  :: m i
  imappend :: m i -> m i -> m i

class TypeIxMonoid (m :: k -> *) where
  type MonoidKind m :: Kind k
  tmempty  :: m (Mempty (MonoidKind m))
  tmappend :: m a -> m b -> m (MappendOf (MonoidKind m) a b)

class TypeIxMonoid m => TypeIxMonoid1 (m :: k -> *) where
  type Munit m (a :: *) :: k
  tmunit :: a -> m (Munit m a)

data List as where
  Nil  :: List '[]
  (:*) :: a -> List as -> List (a ': as)

instance TypeIxMonoid List where
  type MonoidKind List = ListK
  tmempty  = Nil
  tmappend as bs = case as of
    Nil      -> bs
    a :* as' -> a :* tmappend as' bs

instance TypeIxMonoid1 List where
  type Munit List a = '[a]
  tmunit = (:* Nil)

