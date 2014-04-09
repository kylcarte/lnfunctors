{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Control.LnMonad.Focus where

import Data.LnFunctor
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind
import Data.LnFunctor.Mono
import Control.LnApplicative
import Control.LnMonad.Infer
import Type.Families
import Control.Lens
import Data.Proxy
import GHC.Prim (Any)

newtype Focus s t a b r = Focus
  { unFocus :: (ReifiedLens s t a b,r)
  }

type FocusK = K (Any :: Pr * *)

instance IxMono Focus FocusK where
  type Mono Focus FocusK i j = Focus (Fst i) (Snd i) (Fst j) (Snd j)

instance IxFunctorMono Focus FocusK where
  imapMono f (Focus (l,a)) _ = Focus (l,f a)

instance LnFunctorMono Focus FocusK where
  type LMono Focus FocusK i k = i ~ k
  type RMono Focus FocusK j l = j ~ l
  weakenMono     _ (Focus (l,a)) _ = Focus (l,a)
  strengthenMono _ (Focus (l,a)) _ = Focus (l,a)

instance LnApplyMono Focus FocusK where
  type LinkMono Focus FocusK i j k l h m = (h ~ i, j ~ k, l ~ m)
  lapMono _ _ (Focus (l1,f)) (Focus (l2,a)) _ = Focus (l1 `o` l2,f a)

instance LnBindMono Focus FocusK where
  lbindMono _ _ (Focus (l1,a)) f _ = Focus (l1 `o` l2,b)
    where
    Focus (l2,b) = f a

o :: ReifiedLens s t a' b' -> ReifiedLens a' b' a b -> ReifiedLens s t a b
o (Lens l1) (Lens l2) = Lens $ l1 . l2

