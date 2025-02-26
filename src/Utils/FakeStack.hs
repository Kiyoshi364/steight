{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Utils.FakeStack
    ( Stack
    , push , cons, swap, zap, dup, dip, call
    , sons
    , dipped
    -- Short for stackX_1
    , stack0, stack1, stack2, stack3
    -- Short for unstackX_1
    , unstack0, unstack1, unstack2, unstack3
    ) where

type Stack i o = o -> i

push :: a -> forall r . Stack r (a -> r)
push x q = q x

cons :: Stack
    (Stack (a -> r1) r2 -> a -> r)
    (Stack r1 r2 -> r)
cons q f x = q (flip f x)

swap :: Stack (a -> b -> r) (b -> a -> r)
swap = flip

zap :: Stack (a -> r) r
zap = const

dup :: Stack (a -> r) (a -> a -> r)
dup q x = q x x

dip :: Stack (Stack r1 r2 -> a -> r1) (a -> r2)
dip = flip (.)

dipped :: Stack r1 r2 -> Stack (a -> r1) (a -> r2)
dipped = (.)

call :: Stack (Stack r1 r2 -> r1) r2
call = flip id

sons :: Stack
    (Stack (a -> r1) r2 -> a -> r)
    (a -> Stack r1 r2 -> r)
sons = dipped dup . cons . swap

-- Convert Haskell to Stack

stack0 :: a -> forall r . Stack r (a -> r)
stack0 = push

stack1 :: (a -> b) -> forall r . Stack (a -> r) (b -> r)
stack1 f q x = q (f x)

stack2 :: (a -> b -> c) -> forall r . Stack (a -> b -> r) (c -> r)
stack2 f q x y = q (f x y)

stack3 :: (a -> b -> c -> d) -> forall r . Stack (a -> b -> c -> r) (d -> r)
stack3 f q x y z = q (f x y z)

-- Convert Stack to Haskell

unstack0 :: Stack
    ((forall r1 . Stack r1 (a -> r1)) -> r)
    (a -> r)
unstack0 q f = q (f id)

unstack1 :: Stack
    ((forall r1 . Stack (a -> r1) (b -> r1)) -> r)
    ((a -> b) -> r)
unstack1 q f = q (f id)

unstack2 :: Stack
    ((forall r1 . Stack (a -> b -> r1) (c -> r1)) -> r)
    ((a -> b -> c) -> r)
unstack2 q f = q (f id)

unstack3 :: Stack
    ((forall r1 . Stack (a -> b -> c -> r1) (d -> r1)) -> r)
    ((a -> b -> c -> d) -> r)
unstack3 q f = q (f id)
