{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Data.LnFunctor.Copointed where

import Data.LnFunctor

class (LnFunctor w, LnTerminal w) => LnCopointed (w :: ix -> ix -> * -> *) where
  lextract :: Term w i j => w i j a -> a

