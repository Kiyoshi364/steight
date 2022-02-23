module Parsing.Lexer
    ( Tkn(..)
    , Loc(..)
    , Token(..)
    , tokenize
    , parseNum
    , ppTokens
    ) where

import IR.Token
import Data.Bool (bool)
import Utils (fork)

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
    "Parsing.Lexer.string: TODO: handle unclosed string literals"
string sloc l s (c:cs)
    | c == '"'  = Tk sloc (tkstring $ reverse s) : do_tokenize (adv l c) cs
    | c == '\\' = escape sloc l s cs
    | otherwise = string sloc (adv l c) (c:s) cs
  where
    tkstring = TkName . NString

escape :: Loc -> Loc -> String -> String -> [Token]
escape _    _ _  []    = error
    "Parsing.Lexer.escape: handle error: empty backslash"
escape sloc l s (c:cs)
    | c == 't'  = escapeOk '\t' cs
    | c == 'r'  = escapeOk '\r' cs
    | c == 'n'  = escapeOk '\n' cs
    | c == '\\' = escapeOk '\\' cs
    | c == '\'' = escapeOk '\'' cs
    | c == '"'  = escapeOk '\"' cs
    | c == 'x'  = hex cs
    | otherwise = error $ "Parsing.Lexer.escape: handle error : " ++
        "invalid escape sequence: " ++ c:cs ++ " " ++ show l ++ " - " ++ s
  where
    escapeOk e = string sloc l (e:s)
    hex (a:b:bs)
        | isValidHex (a:b:[]) = escapeOk (toEnum $ do_parseHex $ a:b:[]) bs
        | otherwise           = error $ "Parsing.Lexer.escape: " ++
            "handle error : invalid escape sequence in hex"
    hex _  = error $ "Parsing.Lexer.escape: handle error : " ++
        "invalid escape sequence in hex"

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
identify = flip Tk . classify

classify :: String -> Tkn
classify []             = error "Parsing.Lexer.classify: empty String"
classify name@(c:cs)
    | isReserved name  =          reserve   name
    | isUp       c     = TkName $ NUp       name
    | isDown     c     = TkName $ NDown     name
    | isIntro    c     = TkName $ NIntro    name
    | isElim     c     = TkName $ NElim     name
    | isBuiltin  c     = TkName $ NBuiltin  name
    | isSymbol   c     = TkName $ NSymbol   name
    | isValidNum name  = TkName $ NNumber   name
    | isNumOrU   c     = error $
        "Parsing.Lexer.scan.classfy: invalid number: " ++ (c:cs)
    | otherwise        = error $
        "Parsing.Lexer.scan.classfy: unhandled case: " ++ (c:cs)

parseNum :: String -> Int
parseNum  []    = error "Parsing.Lexer.parserNum: empty input"
parseNum num@(c:cs)
    | not (isValidNum num) = error "Parsing.Lexer.parserNum: invalid number"
    | c == '_'  =
        if head cs == '_' then parseNum cs
        else -1 * parseNum cs
    | c == '0'  =
        case head cs of
            'x' -> do_parseHex    $ tail cs
            'o' -> do_parseOctal  $ tail cs
            'b' -> do_parseBinary $ tail cs
            _   -> do_parseNum           cs
    | otherwise =  do_parseNum        (c:cs)

do_parse_model :: Int -> (Char -> Int) -> String -> Int
do_parse_model b f = foldl wrap 0
  where
    wrap n c = if c == '_' then n else b * n + f c

do_parseHex :: [Char] -> Int
do_parseHex = do_parse_model 16 (\ c ->
    if '0' <= c && c <= '9' then fromEnum c - fromEnum '0'
    else if 'a' <= c && c <= 'f' then 10 + fromEnum c - fromEnum 'a'
    else if 'A' <= c && c <= 'F' then 10 + fromEnum c - fromEnum 'A'
    else error $ "Parsing.Lexer.do_parseHex: invalid char: " ++ [c])

do_parseOctal :: [Char] -> Int
do_parseOctal = do_parse_model 8 (\ c -> fromEnum c - fromEnum '0')

do_parseBinary :: [Char] -> Int
do_parseBinary = do_parse_model 2 (\ c -> fromEnum c - fromEnum '0')

do_parseNum :: [Char] -> Int
do_parseNum = do_parse_model 10 (\ c -> fromEnum c - fromEnum '0')

-- Helpers

isIn :: Eq a => [a] -> a -> Bool
isIn list = or . flip map (map (==) list) . flip ($)

isAllIn :: Eq a => [a] -> [a] -> Bool
isAllIn list = foldr ((&&) . isIn list) True

isWhite :: Char -> Bool
isWhite = isIn " \t\r\n"

isUp :: Char -> Bool
isUp = isIn "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

isDown :: Char -> Bool
isDown = isIn "abcdefghijklmnopqrstuvwxyz"

isIntro :: Char -> Bool
isIntro = (== '@')

isElim :: Char -> Bool
isElim = (== '!')

isBuiltin :: Char -> Bool
isBuiltin = (== '#')

isSymbol :: Char -> Bool
isSymbol = isIn "'!@#$%&*-=+\\|/<>:~?,.;"

isNum :: Char -> Bool
isNum = isIn "0123456789"

isNumOrU :: Char -> Bool
isNumOrU = fork (||) isNum (== '_')

isValidNum :: String -> Bool
isValidNum  []    = False
isValidNum (c:cs)
    | c == '_'  =
        if cs /= [] then
            if      head cs == '_' then isValidNum cs
            else if head cs == '0' then isValidNum cs
            else False
        else False
    | c == '0'  =
        if cs /= [] then
            case head cs of
                'x' -> isValidHex               $ tail cs
                'o' -> isValidOctal             $ tail cs
                'b' -> isValidBinary            $ tail cs
                n   -> isNumOrU n && (allNumOrU $ tail cs)
        else True
    | isNum c   = allNumOrU cs
    | otherwise = False
  where
    allNumOrU :: String -> Bool
    allNumOrU = foldr ((&&) . isNumOrU) True

isValidHex :: String -> Bool
isValidHex = isAllIn "_0123456789abcdefABCDEF"

isValidOctal :: String -> Bool
isValidOctal = isAllIn "_01234567"

isValidBinary :: String -> Bool
isValidBinary = isAllIn "_01"

keywords :: [(Tkn, String)]
keywords = [ (TkDo, "do")
    , (TkBlock, "block")
    , (TkType, "type")
    , (TkEnd, "end")
    , (TkDash, "--")
    ]

isReserved :: String -> Bool
isReserved = isIn $ map snd keywords

reserve :: String -> Tkn
reserve s =
    maybe (error $ "Parsing.Lexer.reserve: unexpected reserved word: " ++ s)
        id
    $ foldr (\ p -> maybe (bool Nothing (Just $ fst p) $ snd p == s) Just)
        Nothing keywords

isComment :: String -> Bool
isComment ('/':'/':_) = True
isComment  _          = False