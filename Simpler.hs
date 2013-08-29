{-# LANGUAGE ScopedTypeVariables,ViewPatterns,DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeOperators,TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Simpler where

import Zip
import Data.Typeable
import Control.Monad


newtype MC m a b = MC { runMC :: a -> m b }

type MSeq m a b = FunFT (MC m) a b

data PM m a = forall x. PM (m x) (MSeq (PM m) x a)

instance Monad m => Monad (PM m) where
  return x = PM (return x) empty
  (PM x r) >>= g = PM x (r |> MC g)

advanceMSeq :: Monad m => MSeq (PM m) a b -> a -> m b
advanceMSeq c a = 
     case viewl c of
      EmptyL     -> return a
      ConsL (MC h) r -> 
       case h a of PM x l -> runPM (PM x (l >< r))

runPM :: Monad m => PM m a -> m a
runPM (PM x c) = x >>= advanceMSeq c
  
liftPM :: Monad m => m a -> PM m a
liftPM x = PM x empty

duplicate :: Monad m => m a -> m (m a)
duplicate a = return a

data Co a b c = Yield a (b -> Co a b c) | End c


instance Monad (Co a b) where
  return = End 
  (End a) >>= g = g a
  (Yield a c) >>= g = Yield a $ \x -> c x >>= g

yield x = liftPM $ Yield x return
