module Parsing.Lexer
    ( Tkn(..)
    , Loc(..)
    , Token(..)
    , tokenize
    , parseNum
    , ppTokens
    ) where

import Data.Bool (bool)
import Utils (fork)
import Prelude hiding (getLine)

data Tkn
    -- Parentesis
    = TkOpenPar   | TkClosePar
    | TkOpenBrack | TkCloseBrack
    | TkOpenCurly | TkCloseCurly
    -- Keywords
    | TkDo | TkBlock | TkType | TkEnd | TkDash
    -- Identifiers
    | TkUpIdentifier String
    | TkDownIdentifier String
    | TkSymbolIdentifier String
    | TkNumber String
    -- String Literal
    | TkString String
    -- Comment
    | TkComment String
    deriving (Eq)

instance Show Tkn where
    show TkOpenPar              = "("
    show TkClosePar             = ")"
    show TkOpenBrack            = "["
    show TkCloseBrack           = "]"
    show TkOpenCurly            = "{"
    show TkCloseCurly           = "}"
    show TkDo                   = "Key do"
    show TkBlock                = "Key block"
    show TkType                 = "Key type"
    show TkEnd                  = "Key end"
    show TkDash                 = "Key --"
    show (TkUpIdentifier s)     = "Up "   ++ s
    show (TkDownIdentifier s)   = "Down " ++ s
    show (TkSymbolIdentifier s) = "Symb " ++ s
    show (TkNumber s)           = "Num "  ++ s
    show (TkString s)           = "Str "  ++ s
    show (TkComment s)          = "Comment " ++ s

data Loc = Loc
    { getLine :: Int
    , getCol  :: Int
    }

instance Show Loc where
    show l = show (getLine l) ++ "." ++ show (getCol l)

instance Eq Loc where
    (Loc l1 c1) == (Loc l2 c2) = l1 == l2 && c1 == c2

instance Ord Loc where
    compare (Loc l1 c1) (Loc l2 c2) = case compare l1 l2 of
        LT -> LT
        GT -> GT
        EQ -> compare c1 c2

emptyLoc :: Loc
emptyLoc = Loc 1 1

adv :: Loc -> Char -> Loc
adv (Loc line col) c
    | c == '\n' = Loc (line + 1) 1
    | otherwise = Loc  line      (col + 1)

data Token = Tk Loc Tkn
    deriving (Show)

ppTokens :: [Token] -> String
ppTokens = foldr ( \ t s -> show t ++ "\n" ++ s ) ""

tokenize :: String -> [Token]
tokenize = do_tokenize emptyLoc

do_tokenize :: Loc -> String -> [Token]
do_tokenize _  []    = []
do_tokenize l (c:cs)
    | c == '('  = Tk l TkOpenPar    : do_tokenize (adv l c) cs
    | c == ')'  = Tk l TkClosePar   : do_tokenize (adv l c) cs
    | c == '['  = Tk l TkOpenBrack  : do_tokenize (adv l c) cs
    | c == ']'  = Tk l TkCloseBrack : do_tokenize (adv l c) cs
    | c == '{'  = Tk l TkOpenCurly  : do_tokenize (adv l c) cs
    | c == '}'  = Tk l TkCloseCurly : do_tokenize (adv l c) cs
    | isWhite c =                     do_tokenize (adv l c) cs
    | c == '"'  = string l (adv l c) [] cs
    | isComment (c:cs) = comment l (adv l c) [] (tail cs)
    | otherwise = scan l (adv l c) [c] cs

string :: Loc -> Loc -> String -> String -> [Token]
string _    _ _  []    = error
    "Lexer.string: TODO: handle unclosed string literals"
string sloc l s (c:cs)
    | c == '"'  = Tk sloc (TkString $ reverse s) : do_tokenize (adv l c) cs
    | c == '\\' = escape sloc l s cs
    | otherwise = string sloc (adv l c) (c:s) cs

escape :: Loc -> Loc -> String -> String -> [Token]
escape _    _ _  []    = error
    "Lexer.escape: handle error: empty backslash"
escape sloc l s (c:cs)
    | c == 't'  = string sloc l ('\t':s) cs
    | c == 'r'  = string sloc l ('\r':s) cs
    | c == 'n'  = string sloc l ('\n':s) cs
    | c == '\'' = string sloc l ('\'':s) cs
    | c == '"'  = string sloc l ('\"':s) cs
    | c == 'x'  = error "Lexer.escape: TODO: handle hex escape"
    | otherwise = error "Lexer.escape: handle error : invalid escape sequence"

comment :: Loc -> Loc -> String -> String -> [Token]
comment sloc l s  []    = Tk sloc (TkComment $ reverse s) : do_tokenize l []
comment sloc l s (c:cs)
    | c == '\n' = Tk sloc (TkComment $ reverse s) : do_tokenize (adv l c) cs
    | c == '\r' = comment sloc (adv l c)    s  cs
    | otherwise = comment sloc (adv l c) (c:s) cs

scan :: Loc -> Loc -> String -> String -> [Token]
scan sloc l s  []    = identify (reverse s) sloc : do_tokenize l []
scan sloc l s (c:cs)
    | isWhite c = identify (reverse s) sloc : do_tokenize l (c:cs)
    | c == '"'  = identify (reverse s) sloc : do_tokenize l (c:cs)
    | otherwise = scan sloc (adv l c) (c:s) cs

identify :: String -> Loc -> Token
identify s = case classify s of
    Reserved t -> flip Tk  t
    Up         -> flip Tk $ TkUpIdentifier     s
    Down       -> flip Tk $ TkDownIdentifier   s
    Symbol     -> flip Tk $ TkSymbolIdentifier s
    Number     -> flip Tk $ TkNumber           s

data TkClass = Reserved Tkn | Up | Down | Symbol | Number

classify :: String -> TkClass
classify []             = error "Lexer.classify: empty String"
classify (c:cs)
    | isReserved (c:cs) = reserve (c:cs)
    | isUpper     c     = Up
    | isDown      c     = Down
    | isSymbol    c     = Symbol
    | isNum       c     = Number
    | isNumOrU    c && cs /= [] = Number
    | otherwise         = error $
        "Lexer.scan.classfy: unhandled case: " ++ (c:cs)

parseNum :: String -> Int
parseNum  []    = error "Lexer.parserNum: empty input"
parseNum (c:cs)
    | c == '_'  =
        if cs /= [] then
            if head cs == '_' then parseNum cs
            else -1 * parseNum cs
        else error "Lexer.parserNum: only underscores"
    | otherwise = do_parseNum 0 (c:cs)

do_parseNum :: Int -> [Char] -> Int
do_parseNum a  []    = a
do_parseNum a (c:cs)
    | c == '_'  = rec   a
    | isNum c   = rec $ a * 10 + fromEnum c - fromEnum '0'
    | otherwise = error $ "Lexer.parseNum: invalid char: " ++ show c
  where
    rec = flip do_parseNum cs

-- Helpers

isIn :: Eq a => [a] -> a -> Bool
isIn list = or . flip map (map (==) list) . flip ($)

isWhite :: Char -> Bool
isWhite = isIn " \t\r\n"

isUpper :: Char -> Bool
isUpper = isIn "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

isDown :: Char -> Bool
isDown = isIn "abcdefghijklmnopqrstuvwxyz"

isSymbol :: Char -> Bool
isSymbol = isIn "'!@#$%&*-=+\\|/<>:~?,.;"

isNum :: Char -> Bool
isNum = isIn "0123456789"

isNumOrU :: Char -> Bool
isNumOrU = fork (||) isNum (== '_')

keywords :: [(Tkn, String)]
keywords = [ (TkDo, "do")
    , (TkBlock, "block")
    , (TkType, "type")
    , (TkEnd, "end")
    , (TkDash, "--")
    ]

isReserved :: String -> Bool
isReserved = isIn $ map snd keywords

reserve :: String -> TkClass
reserve s =
    maybe (error $ "Lexer.reserve: unexpected reserved word: " ++ s)
        Reserved
    $ foldr (\ p -> maybe (bool Nothing (Just $ fst p) $ snd p == s) Just)
        Nothing keywords

isComment :: String -> Bool
isComment ('/':'/':_) = True
isComment  _          = False
