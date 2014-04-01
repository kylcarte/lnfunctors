{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.LnComonad
  ( module Control.LnComonad
  -- Convenience Reexports
  , module Data.LnFunctor.Copointed
  , module Data.LnFunctor.Extend
  , module Data.LnFunctor.Empty
  , module Data.LnFunctor.Minus
  ) where

-- Comonad
import Data.LnFunctor.Copointed
import Data.LnFunctor.Coapply
import Data.LnFunctor.Extend

-- ComonadMinus
import Data.LnFunctor.Empty
import Data.LnFunctor.Minus

type LnComonad w = (LnCopointed w, LnExtend w)

-- TODO: icoapLnComonad, iliftW, iliftW2, iliftW3, icosequence, icomapM (?), <fn>_ varieties

{-
icoapLnComonad :: LnComonad w => 
-}

-- }}}

type LnComonadMinus w = (LnComonad w, LnMinus w)

{-
-- LnComonadMinus {{{

iwzero :: LnComonadMinus w => w i i a
iwzero = iempty

iwminus :: LnComonadMinus w => w i j a -> (w i j a,w i j a)
iwminus = icoalt

-- }}}
-}

