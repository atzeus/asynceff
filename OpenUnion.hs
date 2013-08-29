{-# LANGUAGE ScopedTypeVariables,ViewPatterns,DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies,MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}

module Effects (EffReq(..),VE(..),ReqCont, req, weaken, (:>), handle,interpose ) where

import Data.Typeable
import Zip
import Unsafe.Coerce

class Typeable a => EffReq a where
  data Result a

type FunL r e b = FunFT (EffCont r) e b

newtype EffCont r a b = EffCont (a -> VE r b)

data VE r v = V v | E (Eff r v) 

instance Monad (VE r) where
  return = V
  (V a) >>= f = f a
  (E e) >>= f = E (e `andThen` f) where
     andThen (Eff r c) f = Eff r (c |> (EffCont f))

data Eff r a where
   Eff :: EffReq e => e -> FunL r (Result e) a -> Eff r a

data ReqCont r e a = ReqCont e (Result e -> VE r a) deriving Typeable

{-# INLINE req #-}
req :: (EffReq e, Member e r) => e -> Eff r (Result e)
req x = Eff x empty

{-# INLINE prj #-}
prj :: (EffReq e, Member e r) => Eff r a -> Maybe (ReqCont r e a) 
prj (Eff e c) = case cast e of
                  Just a -> Just (ReqCont a (unsafeCoerce $ runFirst c)) 
                  Nothing -> Nothing

{-# INLINE decomp #-}
decomp :: EffReq e => Eff (e :> r) a  -> Either (ReqCont (e :> r) e a)  (Eff (e :> r) a)
decomp a | Just x <- prj a = Left x
decomp a = Right (unsafeCoerce a)

weakenEff :: EffReq e => Eff r a -> Eff (e :> r) a
weakenEff a = unsafeCoerce a

weaken :: EffReq e => VE r a -> VE (e :> r) a
weaken (V x) = V x
weaken (E e) = E (weakenEff e)

infixr 1 :>
data (a  :> b) deriving Typeable

class Member t r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)

runFirst :: FunL r e b -> e -> VE r b
runFirst (viewl -> EmptyL) e    = V e
runFirst (viewl -> ConsL (EffCont h) t) e =
  case h e of
    V x -> runFirst t x
    E e -> E (e `andThens` t) where
     andThens :: Eff r a -> FunL r a b -> Eff r b
     andThens (Eff r c) d = Eff r (c >< d) 

halein f = ha where
  ha (V x) = return x
  ha (E e) = case decomp e of
              Left x -> ha (f x)
              Right (Eff e c) -> E (Eff e onFirst) where
                  onFirst = singleton $ EffCont $ ha . runFirst c

{-# INLINE handle #-}
handle :: EffReq e => (ReqCont (e :> r) e a -> VE (e :> r) a) -> VE (e :> r) a -> VE r a
handle f = halein f

{-# INLINE interpose #-}
interpose :: EffReq e => (ReqCont (e :> r) e a -> VE (e :> r) a) -> VE (e :> r) a -> VE (e :> r) a
interpose f =  halein f


