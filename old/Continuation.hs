{-# LANGUAGE GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable, FlexibleContexts #-}

module Continuation where

import Eff
import OpenUnion
import Data.Typeable
import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.Unique
{--
newtype Cont r a = Cont { run:: (a -> VE r) -> VE r }

data VE w = Val w | E (Int -> VE w)

instance Monad (Cont r) where
  return x = Cont $ \k -> k x
  f >>= g = Cont $ \k -> run f (\x -> run (g x) k)

ask :: Cont r Int
ask = Cont $ \k -> E k

-- runReader :: Cont 
--}

data UniqueNr v = UniqueNr (Int -> v) deriving (Typeable, Functor)

uniqueNr :: Member UniqueNr r => Eff r Int
uniqueNr = send (inj . UniqueNr)

runU :: Eff (UniqueNr :> r) d -> Eff r d
runU m = loop 0 (admin m) where
  loop i (Val x) = return x
  loop i (E u)   = handle_relay u (loop i) $ \(UniqueNr f) -> loop (i+1) (f i)





data Y2 r a d = Done2 d | Y2 a (() -> Eff r (Y2 r a d))

runC2 :: (Typeable a, Typeable d) => Eff (Yield a :> r) d -> Eff r (Y2 r a d)
runC2 m = loop (admin m) where
  loop (Val x) = return (Done2 x)
  loop (E u) = handle_relay u loop $ \(Yield x k) -> return (Y2 x (loop . k))


data EnvOp e v where 
  New :: Typeable a => a -> (Ref e a -> v) -> EnvOp  e v 
  Read :: Typeable a => Ref e a -> (a -> v) -> EnvOp e v
  Write :: Typeable a => Ref e a -> a -> (() -> v) -> EnvOp e v
  deriving (Typeable)
  
instance Functor (EnvOp e) where
  fmap f (New a r) = New a (f . r)
  fmap f (Read r c) = Read r (f . c)
  fmap f (Write r a c) = Write r a (f . c)

data EnvType deriving (Typeable)

newRef :: (Typeable v, Typeable e, Member (EnvOp e) r) => v -> Eff r (Ref e v)
newRef a = send (inj . (\f -> New a f))

readRef :: (Typeable v, Typeable e, Member (EnvOp e) r) => (Ref e v) -> Eff r v
readRef r = send (inj . (\f -> Read r f))

writeRef :: (Typeable v, Typeable e, Member (EnvOp e) r) => (Ref e v) -> v -> Eff r ()
writeRef r a = send (inj . (\f -> Write r a f))


runEnv ::  (forall e. Eff (EnvOp e :> r) a) -> Eff r a
runEnv m = loop emptyEnv (admin m) where
  loop _ (Val x) = return x
  loop s (E u) = handle_relay u (loop s) $ \f -> handle f
    where handle (New a f) = let (s',r) = insertEnv a s
                               in loop s' (f r)
          handle (Read r f) = loop s (f (lookupEnv r s))
          handle (Write r a f) = loop 




-- Private stuff:

data Ref e a = Ref Int

data Env e = Env Int REnv

type REnv = IntMap.IntMap Dynamic

emptyEnv :: Env EnvType
emptyEnv = Env 0 IntMap.empty

insertEnv :: Typeable a => a -> Env e -> (Env e, Ref e a)
insertEnv a (Env n r) = (Env (n + 1) (IntMap.insert n (toDyn a) r), Ref n)

lookupEnv :: Typeable a => Ref e a -> Env e -> a
lookupEnv (Ref i) (Env _ s) | Just a <- fromDynamic (s IntMap.! i) = a


