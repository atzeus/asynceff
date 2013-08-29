{-# LANGUAGE NoMonomorphismRestriction,DeriveDataTypeable, DeriveFunctor, FlexibleContexts, TypeOperators #-}

module LastCoroutine where

import OpenUnion1
import Data.Typeable
import LastEff
import Zip

data Yield a b v = Yield a (b -> v) deriving (Functor,Typeable)

yield
  :: (Typeable a, Typeable b, Member (Yield a b) r) => a -> VE r b
yield a = send (inj $ Yield a id) 

data Y r a b c = Done c | Y a (b -> VE r (Y r a b c))

runYield :: (Typeable a, Typeable b) => VE (Yield a b :> r) c -> VE r (Y r a b c)
runYield (V a) = return $ Done a
runYield (E e c) = 
  case decomp e of
    Right (Yield a c') -> return $ Y a (runYield . processEffects c . c')
    Left e' -> send e' >>= \x -> runYield (processEffects c x)


