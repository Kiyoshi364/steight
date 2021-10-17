module Parser where

import Data.Char (isDigit)
import Control.Applicative

type Input = (Int, String)

-- instance Show Input where show (n, str) = "("++ show n ++"):"++ str
pp :: Input -> String
pp (n, str) = "("++ show n ++"):"++ str

mkInput :: String -> Input
mkInput str = (0, str)

expect :: Char -> Input -> Maybe Input
expect c (i, x:xs)
    | c == x    = Just (i+1, xs)
    | otherwise = Nothing

data Parsed a = Parsed
    { input :: Input
    , value :: Either String a
    }

instance Show a => Show (Parsed a) where
    show (Parsed (n, xs) (Left err)) = "<" ++ show n ++ ": " ++ err      ++ ">"
    show (Parsed  input  (Right a )) = "[" ++ show a ++ ", " ++ pp input ++ "]"

instance Functor Parsed where
    fmap f (Parsed input x) = Parsed input $ fmap f x

newtype Parser a = Parser { runP :: Input -> Parsed a }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \ input -> fmap f $ p input

instance Applicative Parser where
    pure a = Parser $ \ input -> Parsed input $ pure a

    (Parser p1) <*> (Parser p2) = Parser $
        \ input -> case p1 input of
            Parsed input' (Left err) -> Parsed input' $ Left err
            Parsed input' (Right f ) -> fmap f $ p2 input'

instance Alternative Parser where
    empty = Parser $ \ input -> Parsed input $ Left "empty"

    (Parser p1) <|> (Parser p2) = Parser $
        \ input -> case p1 input of
            Parsed _ (Left  _) -> p2 input
            x -> x

charP :: Char -> Parser Char
charP c = Parser $ \ input -> case input of
    (n, []  ) -> Parsed input $ Left $ "expected " ++ [c] ++ ", found <eof>"
    (n, x:xs) -> if c == x
            then Parsed (n+1, xs) $ Right c
            else Parsed input $ Left $ "expected " ++ [c] ++ ", found " ++ [x]

spaceP :: Parser Char
spaceP = charP ' '

wsP :: Parser String
wsP = some $ spaceP <|> charP '\t'

strP :: String -> Parser String
strP = sequenceA . map charP
-- strP = traverse charP -- alternativa

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \ (n, xs) ->
    let (parsed, rest) = span f xs
        in Parsed (n + length parsed, rest) $ Right parsed

numP :: Parser Int
numP = Parser $ \ input ->
    case runP (spanP isDigit) input of
        Parsed input' (Left err) -> Parsed input' $ Left err
        Parsed input' (Right []) -> Parsed input' $ Left "expected number"
        Parsed input' (Right xs) -> Parsed input' $ Right $ read xs

{- TODO
intP :: Parser Int
intP = Parser $ \ input ->
        case runParser (spanP isDigit) input of
            Just (_, [])  -> Nothing
            Just (s, str) -> Just (s, read str)
            _             -> Nothing
-}

optP :: Parser a -> Parser (Maybe a)
optP (Parser p) = Parser $ \ input ->
    case p input of
        Parsed input' (Left  _) -> Parsed input' $ Right $ Nothing
        Parsed input' (Right x) -> Parsed input' $ Right $ Just x
