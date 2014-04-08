
module Data.LnFunctor.Test where

import Data.LnFunctor

data TrivLn (i :: Index) (j :: Index) a = Trv
  { runTrivLn :: a
  } deriving (Eq,Show)

data Index = A | B | C | D | E | F deriving (Eq,Show)

instance IxFunctor TrivLn where
  imap f (Trv a) = Trv $ f a

instance LnFunctor TrivLn where
  type L TrivLn i j = TrivL i j
  type R TrivLn i j = TrivR i j
  weaken (Trv a) = Trv a
  strengthen (Trv a) = Trv a

instance LnInitial TrivLn where
  type Init TrivLn i j = TrivInit i j

instance LnPointed TrivLn where
  lreturn a = Trv a

instance LnApply TrivLn where
  type Link TrivLn i j k l h m = TrivLink i j k l h m
  lap (Trv f) (Trv a) = Trv $ f a

instance LnBind TrivLn where
  lbind (Trv a) f = case f a of
    Trv a -> Trv a

data Tag
  = TLeft
  | TRight
  | TLink
  | TInit
  deriving (Eq,Show)

class TrivL (i :: Index) (j :: Index) where
  trivL :: Proxy (TLeft:*:i:*:j)
  trivL = Proxy

class TrivR (i :: Index) (j :: Index) where
  trivR :: Proxy (TRight:*:i:*:j)
  trivR = Proxy

class TrivLink (i :: Index) (j :: Index) (k :: Index) (l :: Index) (h :: Index) (m :: Index)where
  trivLink :: Proxy (TLink:*:i:*:j:*:k:*:l:*:h:*:m)
  trivLink = Proxy

class TrivInit (i :: Index) (j :: Index) where
  trivInit :: Proxy (TInit:*:i:*:j)
  trivInit = Proxy

instance TrivL i j
instance TrivR i j
instance TrivInit i j
instance TrivLink i j k l h m

