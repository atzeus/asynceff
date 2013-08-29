{-# LANGUAGE ViewPatterns,ScopedTypeVariables,KindSignatures, GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable,FlexibleContexts #-}

module Wait where

import Eff
import OpenUnion
import Data.Typeable
import Control.Applicative






wait :: Member Wait r => Eff r ()
wait =  send (inj . Wait)



data Wait v = Wait (() -> v)  deriving (Typeable,Functor)

data WaitRes r d = Now d | Later (Eff r (WaitRes r d))



runWait :: Eff (Wait :> r) w -> Eff r (WaitRes r w)
runWait m = loop (admin m) where
 loop (Val x) = return (Now x)
 loop (E u)   = handle_relay u loop $ 
                 \(Wait k) -> return $ Later $ (loop . k) ()

runWait' :: Member Wait r => Eff r w -> Eff r (WaitRes r w)
runWait' m = loop (admin m) where
 loop (Val x) = return (Now x)
 loop (E u)   = interpose u loop $ 
                 \(Wait k) -> return $ Later $ (loop . k) ()

relayWait :: Member Wait r => Eff r (WaitRes r w) -> Eff r w
relayWait m = do m <- m
                 case m of
                   Now x -> return x
                   Later f -> wait >> relayWait f

data MRead a v = MRead (a -> v) deriving (Functor, Typeable)

runMRead :: Typeable a =>  a -> Eff (MRead a :> r) b -> Eff r b
runMRead a m = loop (admin m) where
 loop (Val x) = return x
 loop (E u)   = handle_relay u loop $ 
                 \(MRead k) -> loop (k a)

split :: Typeable a => Member Wait r => Eff r a -> Eff (MRead (Maybe a) :> r) b -> Eff r b
split m n = sp (runWait' m) (runWait' n) where
  sp m n = do m <- m
              case m of
                Now a -> runMRead (Just a) $ relayWait n
                Later f -> next f n
  next f n =  do n <- runMRead Nothing n
                 case n of
                     Now a -> return a
                     Later g -> wait >> sp f g

                               
