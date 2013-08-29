{-# LANGUAGE ViewPatterns,ScopedTypeVariables,KindSignatures, GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable,FlexibleContexts #-}


module IOService where

import Control.Monad
import Eff
import OpenUnion
import Data.Typeable


data SIO v = forall a. SIO (IO a) (a -> v) 

instance Functor SIO where
  fmap f (SIO a v) = SIO a (f . v)

instance Typeable1 SIO where
  typeOf1 _ = mkTyConApp tc [] where
      tc = mkTyCon3 "effstuff" "IOSE" "SIO"


io :: Member SIO r => IO a -> Eff r a
io a = Eff $ \x -> E (inj (SIO a x))

runIOService :: Eff (SIO :> Void) a -> IO a
runIOService m = loop (admin m) where
  loop (Val x) = return x
  loop (E (prj -> Just (SIO a x))) = a >>= loop . x

noIO :: Eff r a -> Eff (SIO :> r) a
noIO = weakenEff
