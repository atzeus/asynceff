{-# LANGUAGE FlexibleContexts, TypeFamilies,ViewPatterns,DeriveDataTypeable, TypeOperators #-}

module Demo where

import Eff
import Data.Typeable 
import Control.Monad


data Void deriving Typeable

instance EffReq Void where
  type Result Void = Void

run :: VE Void w -> w
run (V x) = x

data Get e = Get deriving Typeable

instance Typeable e=> EffReq (Get e) where
     type Result (Get e) = e 

ask :: (Typeable e, Member (Get e) r ) => VE r e  
ask = req Get

-- Get = Reader
runGet :: Typeable e => e -> VE (Get e :> r) a -> VE r a
runGet e m = handle (give e) m

give ::  (Member (Get e) r , Typeable e) => e -> ReqC r (Get e) b ->  VE r b
give e (ReqC Get c) = c e


local :: (Typeable e, Member (Get e) r) =>
     (e -> e) -> VE r a -> VE r a 
local f m = do
  e0 <- ask
  let e = f e0
  interpose (give e) m


-- Examples
add :: Monad m => m Int -> m Int -> m Int
add = liftM2 (+)

-- The type is inferred
t1 :: Member (Get Int) r => VE r Int
t1 = ask `add` return (1::Int)

t1' :: Member (Get Int) r => VE r Int
t1' = do v <- ask; return (v + 1 :: Int)

t1r = runGet (10::Int) t1

t1rr = run t1r


-- Inferred type
-- t2 :: (Member (Reader Int) r, Member (Reader Float) r) => Eff r Float

t2 = do
  v1 <- ask
  v2 <- ask
  return $ fromIntegral (v1 + (1::Int)) + (v2 + (2::Float))

-- t2r :: Member (Reader Float) r => Eff r Float
--t2r = runGet (10::Int) t2
-- t2rr :: Eff r Float
t2rr = runGet (20::Float) . runGet (10::Int) $ t2


t2rr' = runGet (20::Int) . runGet (10::Float) $ t2
-- t2rrr = run t2rr
-- 33.0

--t4 :: (Member (Get Int) r, Member (Get Float) r) => VE r Float
--t4 = liftM2 (+) (local (+ (10::Int)) t2) 
            --    (local (+ (30::Float)) t2)


--t4rr = run $ runGet (20::Float) (runGet (10::Int) t4) 
-- 106.0
-- The opposite order of layers gives the same result
--t4rr' = run $ runGet (10::Int) (runGet (20::Float) t4) 
-- 106.0

-- Map an effectful function
-- The type is inferred

tmap = mapM f [1..5]
 where f x = ask `add` return x

tmapr = run $ runGet (10 :: Int) tmap 
-- [11,12,13,14,15]
--}

