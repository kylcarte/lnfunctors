{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnMonad
  ( module Control.LnMonad
  -- Convenience Reexports
  , module Data.LnFunctor.Pointed
  , module Data.LnFunctor.Bind
  , module Data.LnFunctor.Empty
  , module Data.LnFunctor.Plus
  ) where

-- LnMonad
import Data.LnFunctor.Pointed
import Data.LnFunctor.Apply
import Data.LnFunctor.Bind

-- LnMonadPlus
import Data.LnFunctor.Empty
import Data.LnFunctor.Plus

type LnMonad m = (LnPointed m, LnBind m)

{-
-- LnMonad {{{

iapLnMonad :: LnMonad m => m i j (a -> b) -> m j k a -> m i k b
iapLnMonad f x = f >>>= \f' -> x >>>= \x' -> ireturn (f' x')

iliftM :: LnMonad m => (a -> b) -> m i j a -> m i j b
iliftM = iliftA

iliftM2 :: LnMonad m => (a -> b -> c) -> m i j a -> m j k b -> m i k c
iliftM2 = iliftA2

iliftM3 :: LnMonad m => (a -> b -> c -> d)
  -> m i j a -> m j k b -> m k l c -> m i l d
iliftM3 = iliftA3

isequence :: LnMonad m => [m i i a] -> m i i [a]
isequence = foldr (iliftM2 (:)) $ ireturn []

imapM :: LnMonad m => (a -> m i i b) -> [a] -> m i i [b]
imapM f = isequence . map f

isequence_ :: LnMonad m => [m i i a] -> m i i ()
isequence_ = foldr (>>>) $ ireturn ()

imapM_ :: LnMonad m => (a -> m i i b) -> [a] -> m i i ()
imapM_ f = isequence_ . map f

-- }}}
-}

type LnMonadPlus m = (LnMonad m, LnPlus m)

{-
-- LnMonadPlus {{{

imzero :: LnMonadPlus m => m i i a
imzero = iempty

implus :: LnMonadPlus m => m i j a -> m i j a -> m i j a
implus = ialt

-- }}}
-}

