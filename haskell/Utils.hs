module Utils (fork, hook, assert, assertWith, mapFst, mapSnd) where

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) $ g x

hook :: (a -> a' -> b) -> (a -> a') -> a -> b
hook h f x = h x $ f x

assert :: Eq a => a -> (a -> String) -> a -> Either String a
assert = assertWith (==)

assertWith :: (b -> a -> Bool) -> b -> (a -> String) -> a -> Either String a
assertWith eq exp f x
    | eq exp x  = Right $ x
    | otherwise = Left  $ f x

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)
