module Utils
    ( (|>) , (\\) , (|$>) , (\\\) , (...)
    , fork, hook
    , assert, assertWith
    , onFst, onSnd, onBoth, onPair, dup
    , loop
    , Id(..), Default(..)
    , NonEmpty(..), safeCons, asList
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

-- Types

newtype Id a = Id a

instance Functor Id where
    fmap f (Id a) = Id $ f a

instance Applicative Id where
    pure = Id
    (Id f) <*> fa = f <$> fa

instance Monad Id where
    (Id a) >>= f = f a

instance Foldable Id where
    foldr f b (Id a) = f a b

instance Traversable Id where
    traverse f (Id a) = Id <$> f a

class Default e where
    def :: e

instance Default e => Semigroup (Id e) where
    _ <> b = b

instance Default e => Monoid (Id e) where
    mempty = Id def

data NonEmpty a = NonEmpty
    { safeHead :: a
    , safeTail :: [a]
    }

instance Show a => Show (NonEmpty a) where
    show (NonEmpty a as) = show a ++ "::" ++ show as

safeCons :: a -> NonEmpty a -> NonEmpty a
safeCons x = NonEmpty x . asList

asList :: NonEmpty a -> [a]
asList (NonEmpty a as) = a : as

instance Semigroup (NonEmpty a) where
    (NonEmpty a as) <> nbs = NonEmpty a $ as <> asList nbs

instance Functor NonEmpty where
    fmap f (NonEmpty a as) = NonEmpty (f a) $ fmap f as

instance Applicative NonEmpty where
    pure = flip NonEmpty []
    (NonEmpty f fs) <*> (NonEmpty a as) =
        NonEmpty (f a) $ fmap f as <> (fs <*> as)

instance Foldable NonEmpty where
    foldr f b (NonEmpty a as) = f a $ foldr f b as

instance Traversable NonEmpty where
    traverse f (NonEmpty a as) = NonEmpty <$> f a <*> traverse f as
