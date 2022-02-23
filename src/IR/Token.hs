module IR.Token
    ( Name(..)
    , Tkn(..)
    , Loc(..)
    , Token(..)
    , emptyLoc, adv
    , ppTokens
    ) where

import Prelude hiding (getLine)

data Name
    = NUp      String
    | NDown    String
    | NIntro   String
    | NElim    String
    | NBuiltin String
    | NSymbol  String
    | NNumber  String
    | NString  String
    deriving Eq

instance Show Name where
    show (NUp      s) = "Up "       ++ s
    show (NDown    s) = "Down "     ++ s
    show (NIntro   s) = "Intro "    ++ s
    show (NElim    s) = "Elim "     ++ s
    show (NBuiltin s) = "Builtin "  ++ s
    show (NSymbol  s) = "Symb "     ++ s
    show (NNumber  s) = "Num "      ++ s
    show (NString  s) = "Str "      ++ s

data Tkn
    -- Parentesis
    = TkOpenPar   | TkClosePar
    | TkOpenBrack | TkCloseBrack
    | TkOpenCurly | TkCloseCurly
    -- Keywords
    | TkDo | TkBlock | TkType | TkEnd | TkDash
    -- Identifiers and Literals
    | TkName Name
    -- Comment
    | TkComment String
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
    show TkEnd         = "Key end"
    show TkDash        = "Key --"
    show (TkName n)    = show n
    show (TkComment s) = "Comment " ++ s

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
