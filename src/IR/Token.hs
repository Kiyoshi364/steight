module IR.Token
    ( Name(..)
    , Tkn(..)
    , Loc(..)
    , Token(..)
    , fromName
    , emptyLoc, firstLoc, skp, adv, exd, exds, finish
    , canLocMerge, locMerge, assertLocMerge, locSkip, assertLocSkip
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

data TextPos = TextPos
    { getLine :: Int
    , getCol  :: Int
    }

instance Show TextPos where
    show p = show (getLine p) ++ "." ++ show (getCol p)

instance Eq TextPos where
    (TextPos l1 c1) == (TextPos l2 c2) =
        l1 == l2 && c1 == c2

instance Ord TextPos where
    compare (TextPos l1 c1) (TextPos l2 c2) =
        case compare l1 l2 of
            LT -> LT
            GT -> GT
            EQ -> compare c1 c2

emptyTextPos :: TextPos
emptyTextPos = TextPos 1 1

invalidTextPos :: TextPos
invalidTextPos =  (TextPos 0 0)

advTextPos :: TextPos -> Char -> TextPos
advTextPos (TextPos line col) c
    | c == '\n' = TextPos (line + 1)  1
    | otherwise = TextPos  line      (col + 1)

{- First Location should have skip == (1, 1)
 - this.skip should always be equal to prev.end
 - two Locations are "mergeable" if fst.end == snd.skip
 - the token is between start (inclusive) and end (inclusive)
 -}
data Loc = Loc
    { getSkip  :: TextPos
    , getStart :: TextPos
    , getEnd   :: TextPos
    }

instance Show Loc where
    show l = "(" ++ show (getSkip l) ++ ")"
        ++ show (getStart l) ++ "~" ++ show (getEnd l)

instance Eq Loc where
    (Loc sk1 st1 e1) == (Loc sk2 st2 e2) =
        sk1 == sk2 && st1 == st2 && e1 == e2

emptyLoc :: Loc
emptyLoc = Loc invalidTextPos invalidTextPos invalidTextPos

firstLoc :: Loc
firstLoc = Loc emptyTextPos emptyTextPos emptyTextPos

skp :: Loc -> Char -> Loc
skp (Loc sk _ e) c =
    let newEnd = advTextPos e c
    in Loc sk newEnd newEnd

adv :: Loc -> Char -> Loc
adv (Loc _ _ e) c =
    let newEnd = advTextPos e c
    in Loc e newEnd newEnd

exd :: Loc -> Char -> Loc
exd (Loc sk st e) c = Loc sk st (advTextPos e c)

exds :: Loc -> [Char] -> Loc
exds = foldl exd

finish :: Loc -> Loc
finish (Loc _ _ e) = Loc e e e

canLocMerge :: Loc -> Loc -> Bool
canLocMerge l1@(Loc _ _ e1) l2@(Loc sk2 _ _) =
    e1 == sk2 || emptyLoc == l1 || emptyLoc == l2

locMerge :: Loc -> Loc -> Maybe Loc
locMerge l1@(Loc sk1 st1 _) l2@(Loc _ _ e2)
    | emptyLoc == l1    = Just $ l2
    | emptyLoc == l2    = Just $ l1
    | canLocMerge l1 l2 = Just $ Loc sk1 st1 e2
    | otherwise         = Nothing

assertLocMerge :: Loc -> Loc -> Loc
assertLocMerge l1 l2 = maybe (
        error $ "assertLocMerge: "
            ++ "(" ++ show (getSkip l1) ++ ")" ++ show l1
            ++ " (" ++ show (getSkip l2) ++ ")" ++ show l2
    ) id $ locMerge l1 l2

locSkip :: Loc -> Loc -> Maybe Loc
locSkip l1@(Loc sk1 _ _) l2@(Loc _ st2 e2)
    | emptyLoc == l1    = Just $ l2
    | emptyLoc == l2    = Just $ l1
    | canLocMerge l1 l2 = Just $ Loc sk1 st2 e2
    | otherwise         = Nothing

assertLocSkip :: Loc -> Loc -> Loc
assertLocSkip l1 l2 = maybe (
        error $ "assertLocSkip: "
            ++ "(" ++ show (getSkip l1) ++ ")" ++ show l1
            ++ " (" ++ show (getSkip l2) ++ ")" ++ show l2
    ) id $ locSkip l1 l2

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
