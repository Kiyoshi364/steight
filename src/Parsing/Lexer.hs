module Parsing.Lexer
    ( tokenize
    , parseNum
    ) where

import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , firstLoc, skp, adv, exd, exds, finish)
import Data.Bool (bool)
import Utils (fork)

tokenize :: String -> [Token]
tokenize = do_tokenize firstLoc

do_tokenize :: Loc -> String -> [Token]
do_tokenize l  []    = Tk l TkEOF : []
do_tokenize l (c:cs)
    | isPar c   = Tk l (parToken c) : do_tokenize (adv l c) cs
    | isWhite c =                     do_tokenize (skp l c) cs
    | c == '"'  = string (exd l c) [] cs
    | isComment (c:cs) = comment (exds l [c, head cs]) [] (tail cs)
    | otherwise = scan l [c] cs

string :: Loc -> String -> String -> [Token]
string _ _  []    = error
    "Parsing.Lexer.string: TODO: handle unclosed string literals"
string l s (c:cs)
    | c == '"'  = Tk l (tkstring $ reverse s) : do_tokenize (adv l c) cs
    | c == '\\' = escape (exd l c) s cs
    | otherwise = string (exd l c) (c:s) cs
  where
    tkstring = TkName . NString

escape :: Loc -> String -> String -> [Token]
escape _ _  []    = error
    "Parsing.Lexer.escape: TODO: handle error: empty backslash"
escape l s (c:cs)
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
    escapeOk e = string (exd l c) (e:s)
    hex (a:b:bs)
        | isValidHex [a,b] = string (exds l [c,a,b]) ((toEnum $ do_parseHex [a,b]):s) bs
        | otherwise        = error $ "Parsing.Lexer.escape: TODO: " ++
            "handle error : invalid escape sequence in hex"
    hex _  = error $ "Parsing.Lexer.escape: handle error : " ++
        "invalid escape sequence in hex"

comment :: Loc -> String -> String -> [Token]
comment l s  []    = Tk l (TkComment $ reverse s) : do_tokenize (finish l) []
comment l s (c:cs)
    | c == '\n' = Tk l (TkComment $ reverse s) : do_tokenize (adv l c) cs
    | c == '\r' = comment (exd l c)    s  cs
    | otherwise = comment (exd l c) (c:s) cs

scan :: Loc -> String -> String -> [Token]
scan l s  []    = identify (reverse s) l : do_tokenize (finish l) []
scan l s (c:cs)
    | c == '\n'  = identify (reverse s) l : do_tokenize (finish l) (c:cs)
    | shouldStop = identify (reverse s) l : do_tokenize (adv l c ) (c:cs)
    | otherwise  = scan (exd l c) (c:s) cs
  where
    shouldStop  = isPar c || isWhite c || '"' == c

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
    | isTvar     c     = if isValidNum cs
                    then TkName $ NTvar     cs
                    else TkName $ NSymbol   name
    | isTmany    c     = if isValidNum cs
                    then TkName $ NTmany    cs
                    else TkName $ NSymbol   name
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
        case cs of
            ('x':zs) -> do_parseHex    $ zs
            ('o':zs) -> do_parseOctal  $ zs
            ('b':zs) -> do_parseBinary $ zs
            []       -> 0
            _   -> do_parseNum    cs
    | otherwise =  do_parseNum (c:cs)

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

isPar :: Char -> Bool
isPar = isIn "()[]{}"

parToken :: Char -> Tkn
parToken c
    | c == '('  = TkOpenPar
    | c == ')'  = TkClosePar
    | c == '['  = TkOpenBrack
    | c == ']'  = TkCloseBrack
    | c == '{'  = TkOpenCurly
    | c == '}'  = TkCloseCurly
    | otherwise = error $ "Parsing.parToken: invalid parenthesis"

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

isTvar :: Char -> Bool
isTvar = (== '\'')

isTmany :: Char -> Bool
isTmany = (== '%')

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
keywords =
    [ (TkDo, "do")
    , (TkBlock, "block")
    , (TkType, "type")
    , (TkCase, "case")
    , (TkEnd, "end")
    , (TkDash, "--")
    , (TkAdd, "+")
    , (TkSub, "-")
    , (TkSwap, "swap")
    , (TkSwap, "~")
    , (TkRot, "rot")
    , (TkDup, ":")
    , (TkDup, "dup")
    , (TkDrop, ".")
    , (TkDrop, "drop")
    , (TkPrint, "print")
    , (TkApply, "$")
    , (TkHalt, "halt")
    , (TkI64b, "I64")
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
