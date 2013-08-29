{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Jada where


import Control.Monad
import Data.Typeable
import OpenUnion
-- import OpenUnion3

import Data.IORef                       -- For demonstration of lifting

instance Functor ((->) e) where
  fmap = (.)
-- A monadic library for communication between a handler and
-- its client, the administered computation

-- Status of a coroutine (client): done with the value of type w,
-- or sending a request of type Union r
data VE w r = Val w | E !(Union r (VE w r))

-- The Eff monad (not a transformer!)
-- It is actually
--     type Eff r = forall w. Cont (VE w r)
-- We inline it into Cont to put forall under newtype;
-- it is awkward otherwise in Haskell.
-- Also, in MTL, Cont is defined via transformers. We want to
-- avoid transformers!
newtype Eff r a = Eff{runEff :: forall w. (a -> VE w r) -> VE w r}

-- standard instances for a continuation monad
instance Functor (Eff r) where
    fmap f m = Eff $ \k -> runEff m (k . f)

instance Monad (Eff r) where
    {-# INLINE return #-}
    {-# INLINE (>>=) #-}
    return x = Eff $ \k -> k x
    m >>= f  = Eff $ \k -> runEff m (\v -> runEff (f v) k)

-- send a request and wait for a reply
send :: (forall w. (a -> VE w r) -> Union r (VE w r)) -> Eff r a
send f = Eff (E . f)

-- administer a client: launch a coroutine and wait for it
-- to send a request or terminate with a value
admin :: Eff r w -> VE w r
admin (Eff m) = m Val


data Void -- no constructors

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff Void w -> w
run m = case admin m of Val x -> x


-- The request for a value of type e from the current environment
newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = undefined

t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))
