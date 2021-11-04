module Utils
    ( fork, hook
    , assert, assertWith
    , onFst, onSnd, onPair, dup
    , loop
    ) where

-- Combinators

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) $ g x

hook :: (a -> a' -> b) -> (a -> a') -> a -> b
hook h = fork h id

-- Assertions

assert :: Eq a => a -> (a -> String) -> a -> Either String a
assert = assertWith (==)

assertWith :: (b -> a -> Bool) -> b -> (a -> String) -> a -> Either String a
assertWith eq exp f x
    | eq exp x  = Right $ x
    | otherwise = Left  $ f x

-- Pair

onFst :: (a -> c) -> (a, b) -> (c, b)
onFst f (a, b) = (f a, b)

onSnd :: (b -> c) -> (a, b) -> (a, c)
onSnd f (a, b) = (a, f b)

onPair :: (a -> a', b -> b') -> (a, b) -> (a', b')
onPair (fa, fb) = onFst fa . onSnd fb

dup :: a -> (a, a)
dup = hook (,) id

-- Others

loop :: (a -> Either b a) -> a -> (a, b)
loop f x = case f x of
        Right x' -> loop f x'
        Left  b  -> (x, b)
