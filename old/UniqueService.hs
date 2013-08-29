{-# LANGUAGE ViewPatterns,ScopedTypeVariables,KindSignatures, GADTs,TupleSections,TypeOperators, RankNTypes, DeriveFunctor, DeriveDataTypeable,FlexibleContexts #-}

module UniqueService where

import IOService
import Eff
import OpenUnion
import Data.Unique
import Data.Typeable

data GetUnique v = GetUnique (Unique -> v) deriving (Functor, Typeable)

getUnique :: Member GetUnique r => Eff r Unique
getUnique = send (inj . GetUnique)

runUniqueService :: Member SIO r => Eff (GetUnique :> r) w -> Eff r w
runUniqueService m = loop (admin m) where
  loop (Val x) = return x
  loop (E u)   = handle_relay u loop $
                 \(GetUnique c) -> io newUnique >>= loop . c


