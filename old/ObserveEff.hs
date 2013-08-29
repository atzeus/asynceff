{-# LANGUAGE ViewPatterns,ScopedTypeVariables,KindSignatures, FlexibleInstances,GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable, FlexibleContexts #-}

module ObserveEff where

import Eff
import OpenUnion
import Data.Typeable
import Control.Applicative

data Observe (g :: * -> *) (b :: * -> *) (v :: *) =
       Observe { rc :: forall a. (g a, b a -> v) } deriving (Functor)

instance (Typeable1 g, Typeable1 b) => Typeable1 (Observe g b) where
  typeOf1 _ = mkTyConApp tc [tg,tb]
   where tc = mkTyCon3 "dunnoyet" "ObserveEff" "Observe"
         tg = typeOf1 (undefined :: g Void)
         tb = typeOf1 (undefined :: b Void)


runObserve ::  (Typeable1 g, Typeable1 b) =>
           (forall a. g a -> b a) ->
           Eff (Observe g b :> r) d ->
           Eff r d
runObserve f m = loop (admin m) where
 loop (Val x) = return x
 loop (E u)   = handle_relay u loop $
                 \(Observe (r,c)) -> loop (c (f r))


data Unit a = Unit deriving (Typeable, Functor)
data Give e a = Give e deriving (Typeable, Functor)

type OReader e = Observe Unit (Give e)

class AllObserve a
instance AllObserve Void
instance AllObserve a => AllObserve (Observe x y :> a)

-- nice property: f <*> g = pure ap <*> g <*> f where ap g f = f g

instance AllObserve r => Applicative (Eff r) where
  pure = return
  f <*> g = do c <- f ; a <- g ; return (c a)

data Rank a b = LeftWin a | RightWin b | Tie a b

class Race m where
  race :: m a -> m b -> m (Rank a b)

-- send a request and wait for a reply

instance Race (Eff (Wait :> r)) where
  race a b = race' (runWait' a) (runWait' b) where
     race' a b = do { a <- a; b <- b; decide a b }
     decide (Now x) (Now y) = return $ Tie x y
     decide (Now x) _       = return $ LeftWin x
     decide _       (Now y) = return $ RightWin y
     decide (Later a) (Later b) = wait >> race' a b


{--
split :: Member Wait r => Eff r a -> Eff (MRead (Maybe a) :> r) b -> Eff r b
split m n = sp (runWait' m) n where
  sp m n = do m <- m
              case m of
               Now a -> runMRead a n
               Later f -> 
--}




runOReader i = runObserve (\Unit -> Give i)

