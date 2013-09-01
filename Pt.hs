{-# LANGUAGE ScopedTypeVariables,ViewPatterns,DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE TypeOperators,TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Pt where

import Data.Typeable
import Control.Applicative hiding (empty)
import Data.IntMap
import Unsafe.Coerce

data Hide where
  Hide ::  a -> Hide


data Pt s a where
  Pure :: a -> Pt s a
  Ref  :: Int -> Pt s a
  Lam  :: (Pt s a -> Pt s b) -> Pt s (a -> b)
  Ap   :: Pt s (a -> b) -> Pt s a -> Pt s b


instance Functor (Pt s) where
  fmap f g = Ap (Pure f) g

instance Applicative (Pt s) where
  pure   = Pure
  (<*>)  = Ap



runPt :: (forall s. Pt s a) -> a
runPt a = run 0 empty a where
  run :: Int -> IntMap Hide -> Pt s a -> a
  run i e (Pure a) = a
  run i e (Ref j) =  case e ! j of Hide x -> unsafeCoerce x
  run i e (Lam f) = \x -> run i e (Lam f <*>  pure x)
  run i e (Ap (Lam f) g) = let x  = run i e g
                               (e',r) = make i e x
                           in run (i+1) e' (f r)
  run i e (Ap f g)       = (run i e f) (run i e g)
  make :: Int -> IntMap Hide -> a -> (IntMap Hide, Pt s a)
  make i e x = (insert i (Hide x) e, Ref i)

sqr x = x * x
(+.) :: (Applicative a, Num b) => a b -> a b -> a b
a +. b  = pure (+) <*> a <*> b
test = Lam $ \x -> Lam $ \y -> (pure sqr <*> x) +. (pure sqr <*> y)

