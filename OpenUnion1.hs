{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables,KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

-- Open unions (type-indexed co-products) for extensible effects
-- This implementation relies on _closed_ overlapping instances
-- (or closed type function overlapping soon to be added to GHC)

module OpenUnion1 (Union, inj, prj, decomp, 
                   Member, MemberU, MemberU2, (:>), weaken,
                  Without,hide) where

import Data.Typeable
import Zip

-- parameter r is phantom: it just tells what could be in the union
-- This encoding is quite like that in the HList paper.
-- The data constructor Union is not exported

data Union r v where                      -- r is of a kind [*->*]
  Union :: (Functor t, Typeable1 t) => Id (t v) -> Union r v

newtype Id x = Id x                     -- for the sake of gcast1

instance Functor (Union r) where
    {-# INLINE fmap #-}
    fmap f (Union (Id v)) = Union (Id (fmap f v))

{-# INLINE inj #-}
inj :: (Functor t, Typeable1 t, Member t r) => t v -> Union r v
inj x = Union (Id x)

{-# INLINE prj #-}
prj :: (Functor t, Typeable1 t, Member t r) => Union r v -> Maybe (t v)
prj (Union v) | Just (Id x) <- gcast1 v = Just x
prj _ = Nothing

{-# INLINE decomp #-}
decomp :: Typeable1 t => Union (t :> r) v -> Either (Union r v) (t v)
decomp (Union v) | Just (Id x) <- gcast1 v = Right x
decomp (Union v) = Left (Union v)

weaken :: (Typeable1 t, Functor t) => Union r w -> Union (t :> r) w
weaken (Union x) = Union x

class Member (t :: * -> *) r
instance Member t (t :> r)
instance Member t r => Member t (t' :> r)


class Member t r => Without r (t :: * -> *) r' where
  hide :: t v-> Union r' v -> Union r v
  
instance (Typeable1 t, Functor t) => Without (t :> r) t r where
  hide _ a = weaken a

instance forall a r t r'. (Typeable1 a, Functor a, Without r t r',Typeable1 t, Functor t) => 
     Without (a :> r) t (a :> r') where
  hide _ = hid where
   hid a = case decomp a of
    Left x -> weaken $ (hide :: t v -> Union r' v -> Union r v)  undefined x
    Right a -> inj a

-- A sum data type, for `composing' effects
-- In GHC 7.4, we should make it a list
-- (:>) :: (* -> *) -> (* -> List) -> List
infixr 1 :>
data ((a :: * -> *) :> b)

-- This class is used for emulating monad transformers
class Member t r => MemberU (tag :: * -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU tag (tag e) (tag e :> r)
instance MemberU tag t r => MemberU tag t (t' :> r)

-- A version of MemberU for argument of a different kind.
-- Latest GHC has well-functioning PolyKind extension; therefore,
-- MemberU2 can be merged with MemberU.
class Member t r => 
      MemberU2 (tag :: (* -> *) -> * -> *) (t :: * -> *) r | tag r -> t
instance MemberU2 tag (tag e) (tag e :> r)
instance MemberU2 tag t r => MemberU2 tag t (t' :> r)
