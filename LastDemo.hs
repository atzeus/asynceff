{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies #-}

module LastDemo where

import Data.Typeable
import LastEff
import Control.Monad
import OpenUnion1


run :: VE Void a -> a
run (V a) = a

data Reader e v = Reader (e -> v) deriving (Typeable,Functor)

runReader :: Typeable e => VE (Reader e :> r) a ->  e -> VE r a
runReader v e = let rr = handle (\(Reader f) -> rr (f e)) in rr v 

ask :: (Typeable e, Member (Reader e) r) => VE r e
ask = send (Reader id)

local :: (Typeable e, Member (Reader e) r) =>
         (e -> e) -> VE r a -> VE r a
local f = interpose $ \(Reader g) -> ask >>= g . f 

add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

-- The type is inferred
t1 :: Member (Reader Int) r => VE r Int
t1 = ask `add` return (1::Int)

t1' :: Member (Reader Int) r => VE r Int
t1' = do v <- ask; return (v + 1 :: Int)

-- t1r :: Eff r Int
t1r = runReader t1 (10::Int)

t1rr = run t1r


t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
t2r = runReader t2 (10::Int)
-- t2rr :: Eff r Float
t2rr = flip runReader (20::Float) . flip runReader (10::Int) $ t2

t2rrr = run t2rr
-- 33.0

t2rrr' = run $ runReader (runReader t2 (20::Float)) (10::Int)

t3 = t1 `add` local (+ (10::Int)) t1
t3r = run $ runReader t3 (100::Int)

t4
  :: (Member (Reader Int) r, Member (Reader Float) r) =>
     VE r Float
t4 = liftM2 (+) (local (+ (10::Int)) t2) 
                (local (+ (30::Float)) t2)

t4rr = run $ runReader (runReader t4 (10::Int)) (20::Float)
t4rr' = run $ runReader (runReader t4 (20::Float)) (10::Int)
--t4rr = run $ runReader (runReader t4 (10::Int)) (20::Float)

-- 106.0
-- The opposite order of layers gives the same result
--t4rr' = run $ runReader (runReader t4 (20::Float)) (10::Int)

tmap :: Member (Reader Int) r => VE r [Int]
tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = run $ runReader tmap (10::Int)

