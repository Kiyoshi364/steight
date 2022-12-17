module IR.Token
    ( Name(..)
    , Tkn(..)
    , Loc(..)
    , Token(..)
    , fromName
    , emptyLoc, adv
    , ppTokens
    ) where

import Parsing.ParserLib (Match(..))
import Prelude hiding (getLine)

data Name
    = NUp      String       -- starts with uppercase  , is a type
    | NDown    String       -- starts with lowercase  , is anything
    | NIntro   String       -- starts with !          , is a type ctor
    | NElim    String       -- starts with @          , is a type dtor
    | NBuiltin String       -- starts with #          , is a builtin function
    | NTvar    String       -- starts with '          , is a type variable
    | NTmany   String       -- starts with %          , is a type var args
    | NSymbol  String       -- starts with a symbol   , is anything
    | NNumber  String       -- starts with _ or number, is a number
    | NString  String       -- starts with "          , is a string
    deriving Eq

instance Show Name where
    show (NUp      s) = "Up "       ++ s
    show (NDown    s) = "Down "     ++ s
    show (NIntro   s) = "Intro "    ++ s
    show (NElim    s) = "Elim "     ++ s
    show (NBuiltin s) = "Builtin "  ++ s
    show (NTvar    s) = "Tvar "     ++ s
    show (NTmany   s) = "Tmany "    ++ s
    show (NSymbol  s) = "Symb "     ++ s
    show (NNumber  s) = "Num "      ++ s
    show (NString  s) = "Str "      ++ s

instance Match Name where
    match (NUp      _) (NUp      _) = True
    match (NDown    _) (NDown    _) = True
    match (NIntro   _) (NIntro   _) = True
    match (NElim    _) (NElim    _) = True
    match (NBuiltin _) (NBuiltin _) = True
    match (NTvar    _) (NTvar    _) = True
    match (NTmany   _) (NTmany   _) = True
    match (NSymbol  _) (NSymbol  _) = True
    match (NNumber  _) (NNumber  _) = True
    match (NString  _) (NString  _) = True
    match _ _ = False

fromName :: Name -> String
fromName (NUp      s) = s
fromName (NDown    s) = s
fromName (NIntro   s) = s
fromName (NElim    s) = s
fromName (NBuiltin s) = s
fromName (NTvar    s) = s
fromName (NTmany   s) = s
fromName (NSymbol  s) = s
fromName (NNumber  s) = s
fromName (NString  s) = s

data Tkn
    -- Parentesis
    = TkOpenPar   | TkClosePar
    | TkOpenBrack | TkCloseBrack
    | TkOpenCurly | TkCloseCurly
    -- Keywords
    | TkDo | TkBlock | TkType | TkCase | TkEnd | TkDash
    -- Builtins
    | TkAdd | TkSub | TkSwap | TkRot | TkDup
    | TkDrop | TkPrint | TkApply | TkHalt | TkI64b
    -- Identifiers and Literals
    | TkName Name
    -- Comment
    | TkComment String
    -- End of File
    | TkEOF
    deriving (Eq)

instance Show Tkn where
    show TkOpenPar     = "("
    show TkClosePar    = ")"
    show TkOpenBrack   = "["
    show TkCloseBrack  = "]"
    show TkOpenCurly   = "{"
    show TkCloseCurly  = "}"
    show TkDo          = "Key do"
    show TkBlock       = "Key block"
    show TkType        = "Key type"
    show TkCase        = "Key case"
    show TkEnd         = "Key end"
    show TkDash        = "Key --"
    show TkAdd         = "Key +"
    show TkSub         = "Key -"
    show TkSwap        = "Key ~"
    show TkRot         = "Key rot"
    show TkDup         = "Key :"
    show TkDrop        = "Key ."
    show TkPrint       = "Key print"
    show TkApply       = "Key $"
    show TkHalt        = "Key halt"
    show TkI64b        = "Key I64"
    show (TkName n)    = show n
    show (TkComment s) = "Comment " ++ s
    show TkEOF         = "EOF"

instance Match Tkn where
    match TkOpenPar     TkOpenPar     = True
    match TkClosePar    TkClosePar    = True
    match TkOpenBrack   TkOpenBrack   = True
    match TkCloseBrack  TkCloseBrack  = True
    match TkOpenCurly   TkOpenCurly   = True
    match TkCloseCurly  TkCloseCurly  = True
    match TkDo          TkDo          = True
    match TkBlock       TkBlock       = True
    match TkType        TkType        = True
    match TkCase        TkCase        = True
    match TkEnd         TkEnd         = True
    match TkDash        TkDash        = True
    match TkAdd         TkAdd         = True
    match TkSub         TkSub         = True
    match TkSwap        TkSwap        = True
    match TkRot         TkRot         = True
    match TkDup         TkDup         = True
    match TkDrop        TkDrop        = True
    match TkPrint       TkPrint       = True
    match TkApply       TkApply       = True
    match TkHalt        TkHalt        = True
    match TkI64b        TkI64b        = True
    match (TkName n)    (TkName m)    = match n m
    match (TkComment _) (TkComment _) = True
    match TkEOF         TkEOF         = True
    match _ _ = False

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

data Token = Tk
    { loc :: Loc
    , tkn :: Tkn
    }

instance Show Token where
    show (Tk l t) = "Tk " ++ show l ++ ": " ++ show t

instance Match Token where
    match (Tk _ t1) (Tk _ t2) = match t1 t2

ppTokens :: [Token] -> String
ppTokens = foldr ( \ t s -> show t ++ "\n" ++ s ) ""
