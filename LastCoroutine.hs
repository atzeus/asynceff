{-# LANGUAGE RankNTypes, ScopedTypeVariables,GADTs, KindSignatures, NoMonomorphismRestriction,DeriveDataTypeable, DeriveFunctor, FlexibleContexts, TypeOperators #-}

module LastCoroutine where

import OpenUnion1
import Data.Typeable
import LastEff
import Zip
import Control.Applicative
import Control.Monad

{--
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

-}

data Cur a v = Cur (a -> v) deriving (Functor,Typeable)

instance Typeable1 (Cur a) where
  typeOf1 _ = mkTyConApp tc []
        where tc = mkTyCon3 "effstuff" "LastCoroutine" "Cur"

data Update a v = Update a (() -> v) 

data UpRes r a = NC | Up (Maybe a) (Updates r a)

newtype Updates r a = Ups {runUps :: VE r (UpRes r a)} 

instance Functor (UpRes r) where
  fmap f NoChangeEver = NoChangeEver
  fmap f (Up a c) = Up (fmap f a) (fmap f c)

instance Functor (Updates r) where
  fmap f a = Ups $ fmap (fmap f) (runUps a) 

up :: a -> Maybe a -> a
up a Nothing = a
up _ (Just a) = a

comb :: (a -> b) -> Maybe (a -> b) -> a -> Maybe a -> Maybe b
comb a Nothing b Nothing = Nothing
comb a a'      b b'      = Just $ prefRight a a' (prefRight b b')

combineUpRes :: (a -> b) -> a -> UpRes r (a -> b) -> UpRes r a -> UpRes r b
combineUpRes a b = cu where
  cu NC        NC        = NC
  cu x         NC        = fmap (\x -> x b) x
  cu NC        x         = fmap a x
  cu (Up a' f) (Up b' g) = Up e nxt where
        e    = comb a a' b b'
        nxt  = combine (up a a') (up b b') f g 

combine :: (a -> b) -> a -> Updates r (a -> b) -> Updates r a -> Updates r b
combine a b f g = Ups $
   do f' <- runUps f ; g' <- runUps g
      return $ combineUpRes a b f' g'
                          
newtype UV r a = UV { runUV :: VE r (a, Updates r a) }

instance Functor (UV r) where
  fmap f (UV a) = UV $ fmap cont a
    where cont (h,t) = (f h, fmap f t)
      
instance Applicative (UV r) where
  pure x = UV $ return (x, Ups $ return NoChangeEver)
  f <*> g = UV $ do (a,au) <- runUV f ; (b,bu) <- runUV g 
                    return (a b, combine a b au bu)

class ReadOnly (r :: * -> *)
instance ReadOnly (Cur a)

class AllReadOnly r

instance AllReadOnly Void
instance (ReadOnly e, AllReadOnly r) => AllReadOnly (e :> r)





