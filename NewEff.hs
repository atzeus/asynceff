{-# LANGUAGE RankNTypes,ScopedTypeVariables,TypeOperators, FlexibleContexts, DeriveFunctor, DeriveDataTypeable, ViewPatterns, GADTs, ExistentialQuantification #-}

module NewEff where

import Control.Monad
import Data.Typeable
import Zip
import OpenUnion

instance Functor ((->) e) where
  fmap = (.)








data Eff r w = Val w 
            | Comp Eff (FunFT (R (Eff r)) a w)

instance Monad (Eff r) where
   return x = Val x
   (Val x) >>= f = f x
   (Comp u l) >>= f = Comp u (l |> (R f))

comp :: forall r a. Union r (Eff r a) -> Eff r a
comp u = Comp u (empty :: FunFT (R (Eff r)) a a)

send :: forall t r a. (Functor t, Typeable1 t, Member t r) => t (Eff r a) -> Eff r a
send k = comp (inj k) 

handleEff :: forall t r w a. Typeable1 t => (forall a. t (Eff (t :> r) a)  -> Eff r a) -> Eff (t :> r) w -> Eff r w
handleEff f = he where

  he (Val x)                        = return x
  he (Comp (decomp -> Right (e :: t (Eff (t :> r) a )))  (c :: FT (t :> r) a w))  = handle e c
  handle e c = do i <- f e ; he (handleFunFT c i) 

 
--  he (Comp (decomp -> Left u )  c)  = 


handleFunFT :: FT r a w -> a -> Eff r w
handleFunFT (viewl -> EmptyL) a = Val a
handleFunFT (viewl -> ConsL (R h) t) a = 
            case h a of
               Val x -> handleFunFT t x
               Comp u l -> Comp u (l >< t)

{--
nextEff :: Eff r a -> Either a (TUnion r a)
nextEff (Val x) = Left x
nextEff (Comp u c) = Right (fmap (handleFunFT c) u)


handleEff :: (t w  -> Eff r w) -> Eff (t :> r) w -> Eff r w
handleEff f (Val x) = return x
handleEff f (Comp u c) = case decomp u of
                          Left u -> Comp u'  
                          Right u -> f u >>= 





newtype Reader e v = Reader (e -> v)
    deriving (Typeable, Functor)

ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = send (Reader id)

--}
