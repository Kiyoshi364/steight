module Utils (fork, hook) where

fork :: (a1 -> a2 -> b) -> (a -> a1) -> (a -> a2) -> a -> b
fork h f g x = h (f x) $ g x

hook :: (a -> a' -> b) -> (a -> a') -> a -> b
hook h f x = h x $ f x
