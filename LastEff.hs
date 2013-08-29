{-# LANGUAGE ScopedTypeVariables,ViewPatterns,DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE ExistentialQuantification, KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeOperators,TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
module LastEff(Void(..),VE(..), send, handle, interpose,processEffects, VEC(..)) where

import Data.Typeable
import Zip
import Control.Monad
import OpenUnion1

data Void 

data VE r a where
  V :: a -> VE r a
  E :: !(Union r w) -> Effects r w a -> VE r a

instance Monad (VE r) where
  return = V
  (V a) >>= g = g a
  (E e x) >>= g = E e (x |> VEC g)

instance Functor (VE r) where fmap = liftM

send :: Union r a -> VE r a
send u = E u empty



handle :: (Typeable1 e, Functor e) => (e (VE (e :> r) a) -> VE r a) -> VE (e :> r) a -> VE r a
handle f = ha where
  ha (V a) = return a
  ha (E e c) = 
    case decomp e of 
     Right e' -> f (fmap (processEffects c) e')
     Left e' ->  E e' (singleton $ VEC $ ha . processEffects c)

interpose :: (Typeable1 e, Functor e, Member e r) => (e (VE r a) -> VE r a) -> VE r a -> VE r a
interpose f = ha where
  ha (V a) = return a
  ha (E e c) = 
    case prj e of 
     Just e' -> f (fmap (ha . processEffects c) e')
     Nothing -> E e (singleton $ VEC $ ha . processEffects c)


processEffects :: Effects r a b -> a -> VE r b
processEffects (viewl -> EmptyL)    a = return a
processEffects (viewl -> ConsL h t) a = runVEC h a >>= processEffects t

data VEC r a b = VEC { runVEC :: a -> VE r b }

type Effects r a b = FunFT (VEC r) a b

