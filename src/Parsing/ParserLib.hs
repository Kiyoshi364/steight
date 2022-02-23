module Parsing.ParserLib
    ( Stream(..)
    , Parser(..)
    , failP, failWithErrP
    , eofP, eofWithErrP, eofWithErr1P
    , matchP, matchWithErrP, matchWithErr1P, matchWithErr2P
    , matchAnyP, matchAnyWithErrP, matchAnyWithErr1P, matchAnyWithErr2P
    , matchesP, matchesWithErrP, matchesWithErr1P, matchesWithErr2P
    , optP, zeroOrMoreP, oneOrMoreP
    ) where

import Utils (hook, onSnd)
import Utils (NonEmpty(..), asList)
import Control.Applicative (Alternative(..))

class Stream l where
    take_1 :: l t -> Maybe (t, l t)

instance Stream [] where
    take_1  []    = Nothing
    take_1 (x:xs) = Just (x, xs)

data Parser e l t a =
    Parser { runP :: l t -> (l t, Either e a) }

instance Functor (Parser e l t) where
    fmap f (Parser p) = Parser $ \ input -> onSnd (fmap f) $ p input

instance Applicative (Parser e l t) where
    pure a = Parser $ flip (,) $ pure a

    (Parser p1) <*> (Parser p2) = Parser $
        \ input -> case p1 input of
        (input', Right f ) -> onSnd (fmap f) $ p2 input'
        (input', Left err) -> (input', Left err)

instance Monoid e => Alternative (Parser e l t) where
    empty = Parser $ flip (,) $ Left mempty

    (Parser p1) <|> (Parser p2) = Parser $
        \ input -> case p1 input of
        (input1, Right  x ) -> (input1, Right x )
        (_     , Left  er1) -> case p2 input of
            (input2, Right  y ) -> (input2, Right y )
            (_     , Left  er2) -> (input , Left $ er1 <> er2)

failWithErrP :: Stream l => (Maybe t -> e) -> Parser e l t a
failWithErrP err = Parser $ hook (,) (Left . err . fmap fst . take_1)

failP :: Stream l => e -> Parser e l t a
failP = failWithErrP . const

eofWithErr1P :: Stream l => (t -> e) -> Parser e l t ()
eofWithErr1P err = Parser $ \ input -> case take_1 input of
    Just (t, _) -> (input, Left $ err t)
    Nothing     -> (input, Right ())

eofWithErrP :: Stream l => e -> Parser e l t ()
eofWithErrP = eofWithErr1P . const

eofP :: (Monoid e, Stream l) => Parser e l t ()
eofP = eofWithErrP mempty

matchWithErr2P :: (Stream l, Eq t)
    => (t -> Maybe t -> e) -> t -> Parser e l t t
matchWithErr2P err t = Parser $ \ input -> case take_1 input of
    Just (t', s') -> (,) s' $ if t == t' then Right t else Left $ err t $ Just t'
    Nothing       -> (,) input $ Left $ err t Nothing

matchWithErr1P :: (Stream l, Eq t)
    => (Maybe t -> e) -> t -> Parser e l t t
matchWithErr1P = matchWithErr2P . const

matchWithErrP :: (Stream l, Eq t) => e -> t -> Parser e l t t
matchWithErrP = matchWithErr1P . const

matchP :: (Monoid e, Stream l, Eq t) => t -> Parser e l t t
matchP = matchWithErrP mempty

matchAnyWithErr2P :: (Traversable tv, Monoid e, Stream l, Eq t)
    => (tv t -> Maybe t -> e) -> tv t -> Parser e l t t
matchAnyWithErr2P err list =
    (foldr (<|>) empty $ fmap matchP list)
    <|> (failWithErrP $ err list)

matchAnyWithErr1P :: (Traversable tv, Monoid e, Stream l, Eq t)
    => (Maybe t -> e) -> tv t -> Parser e l t t
matchAnyWithErr1P = matchAnyWithErr2P . const

matchAnyWithErrP :: (Traversable tv, Monoid e, Stream l, Eq t)
    => e -> tv t -> Parser e l t t
matchAnyWithErrP = matchAnyWithErr1P . const

matchAnyP :: (Traversable tv, Monoid e, Stream l, Eq t)
    => tv t -> Parser e l t t
matchAnyP = matchAnyWithErrP mempty

matchesWithErr2P :: (Traversable tv, Monoid e, Stream l, Eq t)
    => (tv t -> Maybe t -> e) -> tv t -> Parser e l t (tv t)
matchesWithErr2P err list = (<|>) (traverse matchP list)
    (failWithErrP $ err list)

matchesWithErr1P :: (Traversable tv, Monoid e, Stream l, Eq t)
    => (Maybe t -> e) -> tv t -> Parser e l t (tv t)
matchesWithErr1P = matchesWithErr2P . const

matchesWithErrP :: (Traversable tv, Monoid e, Stream l, Eq t)
    => e -> tv t -> Parser e l t (tv t)
matchesWithErrP = matchesWithErr1P . const

matchesP :: (Traversable tv, Monoid e, Stream l, Eq t)
    => tv t -> Parser e l t (tv t)
matchesP = matchesWithErrP mempty

optP :: (Monoid e) => Parser e l t a -> Parser e l t (Maybe a)
optP p = fmap Just p <|> pure Nothing

zeroOrMoreP :: (Monoid e) => Parser e l t a -> Parser e l t ([] a)
zeroOrMoreP p = fmap asList (oneOrMoreP p) <|> pure []

oneOrMoreP :: (Monoid e) => Parser e l t a -> Parser e l t (NonEmpty a)
oneOrMoreP p = NonEmpty <$> p <*> zeroOrMoreP p

-- untilEof :: (Monoid e, Stream l) => Parser e l t a -> Parser e l t [a]
-- untilEof p = (eofP *> pure []) <|> ((:) <$> p <*> rec)
--     where rec = untilEof p
