
{-# LANGUAGE GADTs #-}


module Zip(FunFT, singleton,(<|), (|>), ViewL(..), ViewR(..), empty, viewl, viewr, (><)) where

data Digit r a b where
  One   :: r a b -> Digit r a b
  Two   :: r a b -> r b c -> Digit r a c
  Three :: r a b -> r b c -> r c d -> Digit r a d
  Four  :: r a b -> r b c -> r c d -> r d e -> Digit r a e

toTree :: Digit r a b -> FunFT r a b
toTree (One a)         = Single a
toTree (Two a b)       = Deep (One a) Empty (One b)
toTree (Three a b c)   = Deep (Two a b) Empty (One c)
toTree (Four a b c d)  = Deep (Two a b) Empty (Two c d)



appendd :: Digit r a b -> Digit r b c -> Digit r a c
appendd (One a)        (One b)        = Two a b
appendd (One a)        (Two b c)      = Three a b c
appendd (Two a b)      (One c)        = Three a b c
appendd (One a)        (Three b c d)  = Four a b c d
appendd (Two a b)      (Two c d)      = Four a b c d
appendd (Three a b c)  (One d)        = Four a b c d


data Node r a b where
  Node2 :: r a b -> r b c -> Node r a c
  Node3 :: r a b -> r b c -> r c d -> Node r a d


data FunFT r a b where
  Empty  :: FunFT r a a
  Single :: r a b -> FunFT r a b
  Deep   :: Digit r a b -> FunFT (Node r) b c -> Digit r c d -> FunFT r a d

infixr 5 <|
(<|) :: r a b -> FunFT r b c -> FunFT r a c
a <| Empty                     = Single a
a <| Single b                  = Deep (One a) Empty (One b) 
a <| Deep (Four b c d e) m sf  = Deep (Two a b) (Node3 c d e <| m) sf
a <| Deep pr m sf              = Deep (appendd (One a) pr) m sf

infixr 5 |>
(|>) :: FunFT r a b -> r b c -> FunFT r a c
Empty                     |> a = Single a
Single b                  |> a = Deep (One b) Empty (One a)
Deep pr m (Four e d c b)  |> a = Deep pr (m |> Node3 e d c) (Two b a)
Deep pr m sf              |> a = Deep pr m (appendd sf (One a))

empty :: FunFT r a a
empty = Empty
singleton :: r a b -> FunFT r a b
singleton a = Single a

infixr 5 ::: 

data FunList r a b where
  FLE :: FunList r a a
  (:::) :: r a b -> FunList r b c -> FunList r a c



toList (One a) = a ::: FLE 
toList (Two a b) = a ::: b ::: FLE
toList (Three a b c) = a ::: b ::: c ::: FLE
toList (Four a b c d) = a ::: b ::: c ::: d ::: FLE


fromList :: FunList r a b -> Digit r a b
fromList (a ::: FLE) = One a
fromList (a ::: b ::: FLE) = Two a b
fromList (a ::: b ::: c ::: FLE) = Three a b c
fromList (a ::: b ::: c ::: d ::: FLE) = Four a b c d


append :: FunList r a b -> FunList r b c -> FunList r a c
append FLE t = t
append (h ::: t) r = h ::: append t r

data ViewL r a b where
  EmptyL :: ViewL r a a
  ConsL  :: r a b -> FunFT r b c -> ViewL r a c

viewl :: FunFT r a c -> ViewL r a c
viewl Empty = EmptyL
viewl (Single a) = ConsL a Empty
viewl (Deep pr m sf) = case toList pr of
            h ::: t -> ConsL h (deepl t m sf)
deepl :: FunList r a b -> FunFT (Node r) b c -> Digit r c d -> FunFT r a d
deepl FLE m sf = case viewl m of
           EmptyL -> toTree sf
           ConsL a m' -> Deep (nodeToDigit a) m' sf 
deepl pr m sf = Deep (fromList pr) m sf

infixr 5 :::< 

data FunListR r a b where
  FLER :: FunListR r a a
  (:::<) :: r b c -> FunListR r a b -> FunListR r a c

toListR :: Digit r a b -> FunListR r a b
toListR (One a) = a :::< FLER
toListR (Two a b) = b :::< a :::< FLER
toListR (Three a b c) = c :::< b :::< a :::< FLER
toListR (Four a b c d) = d:::< c :::< b :::< a :::< FLER


fromListR :: FunListR r a b -> Digit r a b
fromListR (a :::< FLER) = One a
fromListR (b :::< a :::< FLER) = Two a b
fromListR (c :::< b :::< a :::< FLER) = Three a b c
fromListR (d :::< c :::< b :::< a :::< FLER) = Four a b c d


data ViewR r a b where
  EmptyR :: ViewR r a a
  ConsR  ::  FunFT r a b -> r b c -> ViewR r a c

rev = toList . fromListR 

viewr :: FunFT r a c -> ViewR r a c
viewr Empty = EmptyR
viewr (Single a) = ConsR Empty a 
viewr (Deep pr m sf) = case (toListR sf) of
            h :::< t -> ConsR (deepr pr m t) h
deepr :: Digit r a b -> FunFT (Node r) b c -> FunListR r c d -> FunFT r a d
deepr pr m FLER = case viewr m of
           EmptyR -> toTree pr
           ConsR m' a -> Deep pr m' (nodeToDigit a)
deepr pr m sf = Deep pr m (fromListR sf)

nodeToDigit :: Node r a b -> Digit r a b
nodeToDigit (Node2 a b) = Two a b
nodeToDigit (Node3 a b c) = Three a b c

addAlll :: FunList r a b -> FunFT r b c -> FunFT r a c
addAlll FLE m = m
addAlll (h ::: t) m = h <| addAlll t m

addAllr :: FunFT r a b -> FunList r b c  -> FunFT r a c
addAllr m FLE  = m
addAllr m (h ::: t) = addAllr (m |> h) t

  

app3 :: FunFT r a b -> FunList r b c -> FunFT r c d -> FunFT r a d
app3 Empty ts xs = addAlll ts xs
app3 xs ts Empty = addAllr xs ts
app3 (Single x) ts xs = x <| (addAlll ts xs)
app3 xs ts (Single x) = (addAllr xs ts) |> x
app3 (Deep pr1 m1 sf1) ts (Deep pr2 m2 sf2) =
    Deep pr1 
        (app3 m1 (nodes (append (toList sf1) (append ts (toList pr2)))) m2) sf2


nodes :: FunList r a b -> FunList (Node r) a b
nodes (a ::: b ::: FLE) = Node2 a b ::: FLE
nodes (a ::: b ::: c ::: FLE) = Node3 a b c ::: FLE
nodes (a ::: b ::: c ::: d ::: FLE) = Node2 a b ::: Node2 c d ::: FLE
nodes (a ::: b ::: c ::: xs) = Node3 a b c ::: nodes xs

(><) :: FunFT r a b -> FunFT r b c -> FunFT r a c
xs >< ys = app3 xs FLE ys
