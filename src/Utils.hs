module Utils
    ( (|>) , (\\) , (|$>) , (\\\) , (...)
    , fork, hook
    , assert, assertWith
    , onFst, onSnd, onBoth, onPair, dup
    , loop
    ) where

-- Combinators

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
infixl 0 |>

(\\) :: (a -> b) -> (b -> c) -> a -> c
(\\) = flip (.)
infixl 9 \\

(|$>) :: Functor f => f a -> (a -> b) -> f b
(|$>) = flip (<$>)
infixl 4 |$>

(\\\) :: (a1 -> a2 -> b) -> (b -> c) -> a1 -> a2 -> c
(\\\) = flip (...)
infixl 8 \\\

(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) (.) (.)
infixr 8 ...

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) $ g x

hook :: (a -> a' -> b) -> (a -> a') -> a -> b
hook h = fork h id

-- Assertions

assert :: Eq a => a -> (a -> String) -> a -> Either String a
assert = assertWith . (==)

assertWith :: (a -> Bool) -> (a -> String) -> a -> Either String a
assertWith p f x
    | p x       = Right $ x
    | otherwise = Left  $ f x

-- Pair

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

onBoth :: (a -> b) -> (a, a) -> (b, b)
onBoth f (a, b) = (f a, f b)

onPair :: (a -> a', b -> b') -> (a, b) -> (a', b')
onPair (fa, fb) = onFst fa . onSnd fb

dup :: a -> (a, a)
dup = hook (,) id

-- Others

loop :: (a -> Either b a) -> a -> (a, b)
loop f x = case f x of
        Right x' -> loop f x'
        Left  b  -> (x, b)
