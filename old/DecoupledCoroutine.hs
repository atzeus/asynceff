{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, ViewPatterns #-}
{-# LANGUAGE MagicHash,KindSignatures,DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes,ImpredicativeTypes, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

module DecoupledCoroutine where


import Debug.Trace
import Data.Dynamic
import GHC.Exts
import Prelude hiding (length)

                              





      
-- innstance 


data T a = Done a | Delay (FunFT (R T) () a)

instance Monad T where
  return = Done
  (Done a) >>= f = f a
  (Delay f) >>= g =  trace "composing!"  $ Delay (f |> (R g))

delay f = Delay (singleton (R $ \_ -> f))

interpret (Done a) = a
interpret (Delay f) = interpret (interpretFT () f)

interpretFT :: a -> FunFT (R T) a b -> T b
interpretFT a (viewl -> EmptyL) = return a
interpretFT a (viewl -> ConsL (R h) t) =
  case h a of
      Done b -> interpretFT b t
      Delay l -> Delay (l >< t)

mid x = return x

test x = (((delay $ delay $ delay $ return x) >>= mid) >>= mid) >>= mid
test1 = test 1
test2 = test 2

data To a = Doneo a | Delayo (() -> To a)

instance Monad To where
  return = Doneo
  (Doneo a) >>= f = f a
  (Delayo f) >>= g = trace "composing!" $ Delayo ((>>= g) . f)

delayo f = Delayo (\_ -> f)



runT (Doneo a) = a
runT (Delayo f) = runT (f ())

testo x = (((delayo $ delayo $ delayo $ return x) >>= mid) >>= mid) >>= mid
testo1 = testo 1
testo2 = testo 2

{--
delayp :: Tp a -> Tp a
delayp t = Delayp $ insertFL (\_ -> t)


testp x = (((delayp $ delayp $ delayp $ return x) >>= mid) >>= mid) >>= mid
testp1 = testp 1
testp2 = testp 2

data Tp a = Donep a | Delayp (FL () a)

data Void

type Rany = Any Void

data FL a b where
  FL :: Seq (Rany -> Tp Rany) -> FL a b

toR f = (\x -> unsafeCoerce# (f (unsafeCoerce# x :: a)) :: Tp Rany)
insertFL :: (a -> Tp b) -> FL a b
insertFL f = FL $ singleton (toR f)

addFL :: FL a b -> (b -> Tp c) -> FL a c
addFL (FL f) g = FL (f |> toR g)

instance Monad Tp where
  return = Donep
  (Donep a) >>= f = f a
  (Delayp f) >>= g = trace "composing!" $ Delayp $ addFL f g

runTp :: Tp a -> a
runTp (Donep a) = a
runTp (Delayp t) = runTp (runFL () t)


runFL :: forall a b. a -> FL a b -> Tp b
runFL a (FL t) = rf' (unsafeCoerce# a :: Rany) t where
   rf' :: Rany -> Seq (Rany -> Tp Rany) -> Tp a
   rf' a l = rf a l
   rf :: Rany -> Seq (Rany -> Tp Rany) -> Tp a
   rf a (viewl -> f :< t) = case f a of
                              Donep a ->  rf' a t
                              Delayp (FL f) -> (unsafeCoerce# $ Delayp (FL (f >< t)) :: Tp a)
   rf a (viewl -> EmptyL) = Donep (unsafeCoerce# a :: a)

--}







{--
delay :: T a -> T a
delay t = Delay $ insertFL (\_ -> t)

mid x = return x

test = (((delay $ delay $ return 1) >>= mid) >>= mid) >>= mid
runT :: T a -> a
runT (Done a) = a
runT (Delay f) = runT (f ())
--}

         





{--
data C m a b = C (forall x z. Seq (x -> m z)

unsafeMakeC :: (a -> m b) -> C m
unsafeMakeC f = C (unsafeCoerce f)

data FM m a b = FM (Seq (C m)) 

toMR :: (a -> m b) -> FM m a b
toMR f = FM (singleton (unsafeMakeC f))



instance (Monad m, Extract m) => Monad (FM m a) where
  return x = toMR (\_ -> return x)
  (FM x) >>= g = FM 


--}



{--
unsafeRunBinds :: forall a b r. => [Bind r] -> a -> (VE r b, [Bind r])
unsafeRunBinds (h : t) a = let r = unsafeRunBind h a
                           in case r of
                               Val x => unsafeRunBinds t x
                               E r => ( 
--}

{--
data Mon m b = forall a. Mon (a,[Bind m])



unsafeRunMon :: Mon m b -> m b
unsafeRunMon (Mon a, l) = runThrough l a where
   runThrough ((Bind h) : t) = 
--}

{--
data Cont a = Cont { runCont :: forall e. (a -> e) -> e }

toCont :: a -> Cont a
toCont a = Cont $ \f -> f a

fromCont :: Cont a -> a
fromCont (Cont a) = a id

andThen :: (forall e. (a -> e) -> e) -> (a -> b) -> (forall e. (b -> e) -> e)
andThen a f = \x -> x (a f)

instance Functor Cont where
  fmap f (Cont a) = Cont $ \x -> x (a f)

instance Monad Cont where
  return a = Cont $ \f -> f a
  (Cont f) >>= g = Cont $ \x -> let Cont g' = f g in g' x

data Id a = Id a | Yield (Id a)

instance Functor Id where
  fmap f (Id a) = Id (f a)
  fmap f (Yield a) = Yield (fmap f a)

instance Monad Id where
  return = Id
  (Id a) >>= f = f a
  (Yield a) >>= f = Yield (a >>= f)
--}
--import Eff

{--
import OpenUnion
import Data.Typeable


data Eff r a =
  Val  a  |
  Send (Union r (Eff r a)) 


instance Functor (Eff r) where
  fmap f (Val a) = Val (f a)
  fmap f (Send a) = Send (fmap (fmap f) a)

instance Monad (Eff r) where
  return = Val
  (Val a) >>= f = f a
  (Send a) >>= f = Send (fmap (>>= f) a)

send = Send

data Void 

run :: Eff Void w -> w
run (Val a) = a

newtype Reader e v = Reader (e -> v) deriving (Typeable,Functor)

ask :: (Typeable e, Member (Reader e) r) => Eff r e
ask = Send (inj (Reader return ))
  
handleRelay :: Typeable1 t =>
     Union (t :> r) v -> (v -> Eff r a) -> (t v -> Eff r a) -> Eff r a
handleRelay u loop h = case decomp u of
  Right x -> h x
  Left u  -> Send (fmap loop u)

runReader :: Typeable e => e ->  Eff (Reader e :> r) v -> Eff r v
runReader i = loop where
  loop (Val a) = return a
  loop (Send b) = handleRelay b loop (\(Reader f) -> loop (f i))

--runReader i (Eff 
           
--}         
{--
data Emit a v = Emit a (() -> v) deriving (Typeable, Functor)

data EmitOut r a d = EmitReturn d | Emitted a (Eff r (EmitOut r a d))

emit :: (Typeable a, Member (Emit a) r) => a -> Eff r ()
emit a = send (inj . Emit a)

runEmit :: Typeable a => Eff (Emit a :> r) d -> Eff r (EmitOut r a d)
runEmit m = loop (admin m) where
 loop (Val x) = return (EmitReturn x)
 loop (E u)   = handle_relay u loop $ 
                 \(Emit a k) -> return $ Emitted a $ loop (k ())


data Request a v = Request (a -> v) deriving (Typeable,Functor)

data RequestOut r a d = RequestReturn d | Requested (a -> Eff r (RequestOut r a d))

runAwait :: Typeable a => Eff (Request a :> r) d -> Eff r (RequestOut r a d)
runAwait m = loop (admin m) where
 loop (Val x) = return (RequestReturn x)
 loop (E (u)   = handle_relay u loop $ 
                 \(Request f) -> return $ Requested (loop . f)
--}

