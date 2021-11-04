module Parser where

import Data.Char (isDigit)
import Control.Applicative

type Column = Int
type Lines = Int
data Loc = L FilePath Lines Column

instance Show Loc where
    show (L path ln cn) = path++"."++show ln++"."++ show cn

mkLoc :: FilePath -> Loc
mkLoc path = L path 1 1

inc :: Loc -> Char -> Loc
inc (L p ln cn) x
    | x == '\r' = L p  ln     cn
    | x == '\n' = L p (ln+1)   1
    | otherwise = L p  ln    (cn+1)

data Input = I
    { loc    :: Loc
    , string :: String
    }

instance Show Input where
    show (I lc str) = show lc ++":"++ str

mkPathInput :: FilePath -> String -> Input
mkPathInput = I . mkLoc

mkInput :: String -> Input
mkInput = mkPathInput ""

nextChar :: Input -> String
nextChar (I _  []  ) = "<eof>"
nextChar (I _ (x:_)) = [x]

expect :: Char -> Input -> Maybe Input
expect _ (I _  []   ) = Nothing
expect c (I l (x:xs))
    | c == x          = Just (I (inc l x) xs)
    | otherwise       = Nothing

expects :: String -> Input -> Maybe Input
expects  []    = Just
expects (x:xs) = (>>= expects xs) . expect x

data Parsed a = Parsed
    { inputP :: Input
    , value :: Either String a
    }

instance Show a => Show (Parsed a) where
    show (Parsed (I l _) (Left err)) = "<"++ show l ++": "++ err        ++">"
    show (Parsed  input  (Right a )) = "["++ show a ++", "++ show input ++"]"

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

instance Monad Parser where
    (Parser p1) >>= f = Parser $
        \ input -> case p1 input of
            Parsed input' (Left  err) -> Parsed input' (Left  err)
            Parsed input' (Right  x ) -> runP (f x) input'

failP :: String -> Parser a
failP s = Parser $ \ input -> Parsed input $ Left s

eofP :: Parser ()
eofP = Parser $ \ input -> case input of
    I _ [] -> Parsed input $ Right ()
    I _ xs -> Parsed input $ Left $ "expected <eof>, found `" ++ xs ++ "`"

charP :: Char -> Parser Char
charP c = Parser $ \ input -> case expect c input of
    Just (I l xs) -> Parsed (I l xs) $ Right c
    Nothing       -> Parsed input $ Left $
        "expected `" ++ [c] ++ "`, found `" ++ nextChar input ++ "`"

spaceP :: Parser Char
spaceP = charP ' '

wsP :: Parser String
wsP = some $ spaceP <|> charP '\t'

lfP :: Parser String
lfP = some $ charP '\n' <|> charP '\r'

whiteP :: Parser String
whiteP = concat <$> many (wsP <|> lfP)

strP :: String -> Parser String
strP str = (<|>) (sequenceA $ map charP str)
    (wordP >>= failP .
        (\s -> "expected `" ++ str ++ "`, found `" ++ s ++ "`"))
-- strP = traverse charP -- alternativa

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \ input@(I _ xs) ->
    let (parsed, _) = span f xs
        input' = case expects parsed input of
                Just inp -> inp
                Nothing  -> error "This should not happen!"
        in Parsed input' $ Right parsed

wordP :: Parser String
wordP = spanP $ and . flip map (map (/=) [' ', '\t', '\r', '\n']) . flip ($)

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

untilEof :: Parser a -> Parser [a]
untilEof p = (eofP *> pure []) <|> ((:) <$> p <*> rec)
    where rec = untilEof p
