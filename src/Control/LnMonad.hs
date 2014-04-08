{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnMonad where

import Data.LnFunctor
import Data.LnFunctor.Bind
import Data.LnFunctor.Plus
import Control.LnApplicative
import Prelude hiding (return)

type LnMonad f = (LnApplicative f, LnBind f)

lreturn :: (LnMonad f, Init f i j) => a -> f i j a
lreturn = lpure

return :: (LnMonad f, Init f i i) => a -> f i i a
return = lreturn

class (LnMonad f, LnPlus f) => LnMonadPlus f where
  lzero :: Init f i j => f i j a

mzero :: (LnMonadPlus f, Init f i i) => f i i a
mzero = lzero

