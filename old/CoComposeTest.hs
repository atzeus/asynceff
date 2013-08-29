{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, ViewPatterns #-}
{-# LANGUAGE MagicHash,KindSignatures,DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes,ImpredicativeTypes, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module CoComposeTest where

import Data.Sequence
import Data.Typeable
import Debug.Trace
import Data.Dynamic
import GHC.Exts
import Prelude hiding (length)

data T a = Done a | Delay (() -> T a)

instance Monad T where
  return = Done
  (Done a) >>= f = f a
  (Delay f) >>= g = trace "composing!" $ Delay ((>>= g) . f)

delay f = Delay (\_ -> f)

mid x = return x

runT (Done a) = a
runT (Delay f) = runT (f ())

test x = (((delay $ delay $ delay $ return x) >>= mid) >>= mid) >>= mid
test1 = test 1
test2 = test 2


delayp :: Tp a -> Tp a
delayp t = Delayp $ insertFL (\_ -> t)


testp x = (((delayp $ delayp $ delayp $ return x) >>= mid) >>= mid) >>= mid
testp1 = testp 1
testp2 = testp 2

data Tp a = Donep a | Delayp (FL () a)

data Void

type Rany = Any Void

data FL a b where
  FL :: Seq (Rany -> Tp Rany) -> FL a b

toR f = (\x -> unsafeCoerce# (f (unsafeCoerce# x :: a)) :: Tp Rany)
insertFL :: (a -> Tp b) -> FL a b
insertFL f = FL $ singleton (toR f)

addFL :: FL a b -> (b -> Tp c) -> FL a c
addFL (FL f) g = FL (f |> toR g)

instance Monad Tp where
  return = Donep
  (Donep a) >>= f = f a
  (Delayp f) >>= g = trace "composing!" $ Delayp $ addFL f g

runTp :: Tp a -> a
runTp (Donep a) = a
runTp (Delayp t) = runTp (runFL () t)


runFL :: forall a b. a -> FL a b -> Tp b
runFL a (FL t) = rf' (unsafeCoerce# a :: Rany) t where
   rf' :: Rany -> Seq (Rany -> Tp Rany) -> Tp a
   rf' a l = rf a l
   rf :: Rany -> Seq (Rany -> Tp Rany) -> Tp a
   rf a (viewl -> f :< t) = case f a of
                              Donep a ->  rf' a t
                              Delayp (FL f) -> (unsafeCoerce# $ Delayp (FL (f >< t)) :: Tp a)
   rf a (viewl -> EmptyL) = Donep (unsafeCoerce# a :: a)
