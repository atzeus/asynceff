{-# LANGUAGE ViewPatterns,ScopedTypeVariables,KindSignatures, FlexibleInstances,GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable, FlexibleContexts,MultiParamTypeClasses,UndecidableInstances, FunctionalDependencies, TypeOperators,NoMonomorphismRestriction,
IncoherentInstances #-}

module PRef(PRef, PRefOp, createPR, readPR,modPR, NRPRef(..)) where

import OpenUnion1
import Data.Typeable
import Data.Dynamic
import Data.IntMap 
import LastEff

-- type level number for nesting depth

data Z   deriving Typeable
data Suc n deriving Typeable

-- n is the level, using the above type level number, s is supposed to be forall'ed so that reference cannot escape (ala ST)

data PRef n s a where
  PRef :: Typeable a => Int -> PRef n s a

data PRefOp n s v 
  = forall a . Typeable a => Create a (PRef n s a -> v)
  | forall a . Read (PRef n s a) (a -> v) 
  | forall a . Mod  (PRef n s a) a (() -> v) 

instance Functor (PRefOp n s) where
  fmap f (Create a g) = Create a (f . g)
  fmap f (Read r g) = Read r (f . g)
  fmap f (Mod r a g) = Mod r a (f . g)

createPR :: (Typeable a, Typeable n, Member (PRefOp n s) r) => a -> VE r (PRef n s a)
createPR a = send (Create a id)

readPR :: (Typeable a, Typeable n, Member (PRefOp n s) r) => PRef n s a -> VE r a
readPR r = send (Read r id)

modPR :: (Typeable a, Typeable n, Member (PRefOp n s) r) => PRef n s a -> a -> VE r ()
modPR r a = send (Mod r a id)


-- ignore forall'ed s type, not safe in general, but safe for our use here

instance forall n s . Typeable n => Typeable1 (PRefOp n s) where
   typeOf1 _ = mkTyConApp tc [tg] where
      tc = mkTyCon3 "effstuff" "PRef" "PRefOp"
      tg = typeOf (undefined :: n)



rPref :: forall n r a. Typeable n => (forall s. VE (PRefOp n s :> r) a) -> VE r a
rPref v = handPRef' empyEnv v where
  handPRef' s = handle (handPRef s)
  handPRef s (Create a f) = handPRef' s' $ f r
           where (s',r) = prefCreate s a
  handPref s (Read r f) = handPRef' s $  f $ prefGet r s
  handPref s (Mod r a f) = handPRef' s' $  f ()
             where s' = prefPut r a s



class Typeable n => NRPRef r n | r -> n where
  runPRef :: (forall s. VE (PRefOp n s :> r) a) -> VE r a

instance NRPRef Void Z where
  runPRef = rPref

instance forall r n s. NRPRef r n => NRPRef (PRefOp n s :> r) (Suc n) where
  runPRef = rPref

instance forall t r n . NRPRef r n => NRPRef (t :> r) n where
  runPRef = rPref

data Env n s = Env {count :: Int, map :: (IntMap Dynamic)}

empyEnv = Env 0 empty


prefCreate :: Typeable a => Env n s -> a -> (Env n s, PRef n s a)
prefCreate (Env n s) a = (Env (n+1) (insert n (toDyn a) s), PRef n)

prefGet :: PRef n s a -> Env n s -> a
prefGet (PRef u) (Env n s) | Just a <- fromDynamic (s ! u) = a

prefPut :: PRef n s a -> a -> Env n s -> Env n s
prefPut (PRef u) a (Env n s) = Env n (insert u (toDyn a) s)

