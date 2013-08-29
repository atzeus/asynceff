{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}


module Effects where

import LastEff 
import LastEff
import Control.Monad

data Void deriving Typeable
instance Eff Void where type Res Void = Void

run :: VE Void a -> a
run (V a) = a

data State e = Get 
             | Put e deriving Typeable
in

