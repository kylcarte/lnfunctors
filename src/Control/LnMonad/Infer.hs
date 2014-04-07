{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Control.LnMonad.Infer where

import Data.LnFunctor
import Type.Families
import Data.Proxy

type Thm a = Infer '[] '[a] ()
type Thms as = Infer '[] as ()

data a :=: b where
  Refl :: a :=: a
infixr 3 :=:

give :: IsProof a => a -> Thm a
give a = Infer $ const (c a,())

refl :: Thm (a :=: a)
refl = give Refl

data Cxt as where
  NilC  :: Cxt '[]
  ConsC :: IsProof a => a -> Cxt as -> Cxt (a ': as)

c :: IsProof a => a -> Cxt '[a]
c = (`ConsC` NilC)

newtype Infer i j a = Infer
  { runInfer :: Cxt i -> (Cxt j,a)
  }

class IsProof (a :: *) where
  type Presume a :: Constraint
  proof :: a -> Proof (Presume a)

instance IsProof (a :=: b) where
  type Presume (a :=: b) = a ~ b
  proof Refl = Proof

instance IsProof (Cxt as) where
  type Presume (Cxt as) = PresumeAll as
  proof as = case as of
    NilC          -> Proof
    a `ConsC` as' -> case (proof a,proof as') of
     (Proof,Proof) -> Proof

(&) :: (IsProof a, IsProof b) => a -> b -> Proof (Presume a,Presume b)
a & b = case (proof a,proof b) of (Proof,Proof) -> Proof
infixr 5 &

(|-) :: Proof c -> (c => r) -> r
Proof |- r = r

type family PresumeAll as :: Constraint where
  PresumeAll '[]       = ()
  PresumeAll (a ': as) = (Presume a,PresumeAll as)

data Proof c where
  Proof :: c => Proof c

{-
addZ :: N a -> Thm (a :+ Z :=: a)
addZ a = case a of
  NZ    -> refl
  NS a' -> by $ addZ a'
-}

