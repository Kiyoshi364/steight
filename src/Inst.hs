module Inst
    ( AST(..)
    , Builtin(..)
    , Inst(..)
    , ipp
    , instTyp
    , lexer
    ) where

import Parser
import Control.Applicative
import Types (TypeSig(..), ConstT(..))
import Types.Parsing (typeP)
import qualified Utils
import Dict (Dict)

data AST = AST
    { dict :: Dict String (Maybe TypeSig, [Inst])
    }

instance Show AST where show (AST ds) = "AST " ++ show ds

ipp :: Show a => [a] -> String
ipp = foldr (\ i s -> show i ++ " " ++ s) ""

data Builtin
    = Add
    | Sub
    | Swap
    | Dup
    | Drop
    | Print
    | Halt
    | Apply
    deriving Eq

instance Show Builtin where
    show (Add   ) = "+"
    show (Sub   ) = "-"
    show (Swap  ) = "~"
    show (Dup   ) = ":"
    show (Drop  ) = "."
    show (Print ) = "print"
    show (Apply ) = "$"
    show (Halt  ) = "halt"

data Inst
    = Push Int
    | Builtin Builtin
    | PQuote [Inst]
    | Doblk [Inst]
    | Nameblk String [Inst]
    | Typblk TypeSig [Inst]
    | NameTypblk String TypeSig [Inst]
    | Identifier String
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Builtin b) = show b
    show (PQuote is) = "#[ " ++ ipp is ++ "]"
    show (Doblk  is) = "do " ++ ipp is ++ "end"
    show (Typblk typ is) = "do <" ++ show typ ++ "> " ++
        ipp is ++ "end"
    show (Nameblk name is) = "block " ++ name ++ " " ++ ipp is ++ "end"
    show (NameTypblk name typ is) = "block " ++ name ++ " <" ++
        show typ ++ "> " ++ ipp is ++ "end"
    show (Identifier ref) = "{" ++ ref ++ "}"

lexer :: Parser AST
lexer = fmap AST $ some (
    fmap (\i -> case i of
        Nameblk    s     is -> (s, (Nothing , is))
        NameTypblk s typ is -> (s, (Just typ, is))
        _ -> error "Inst.lexer: non-exhaustive pattern")
    (whiteP *> commentP *> (nametypblkP <|> nameblkP))
    ) <* whiteP <* commentP <* eofP

instP :: Parser Inst
instP = commentP *> whiteP *>
    (   pushP
    <|> builtinP
    <|> quotedP
    <|> typblkP <|> doblkP
    <|> nametypblkP <|> nameblkP
    <|> identifierP
    <|> errP)

pushP :: Parser Inst
pushP = fmap Push numP

builtinP :: Parser Inst
builtinP = fmap Builtin $
        addP   <|> subP
    <|> swapP  <|> dupP  <|> dropP
    <|> printP <|> haltP
    <|> applyP

addP :: Parser Builtin
addP = charP '+' *> pure Add

subP :: Parser Builtin
subP = charP '-' *> pure Sub

swapP :: Parser Builtin
swapP = (strP "swap" <|> strP "~") *> pure Swap

dupP  :: Parser Builtin
dupP  = (strP "dup" <|> strP ":") *> pure Dup

dropP :: Parser Builtin
dropP = (strP "drop" <|> strP ".") *> pure Drop

printP :: Parser Builtin
printP = strP "print" *> pure Print

haltP :: Parser Builtin
haltP = strP "<>" *> pure Halt

applyP :: Parser Builtin
applyP = (strP "apply" <|> strP "$") *> pure Apply

quotedP :: Parser Inst
quotedP = fmap PQuote
    (strP "#[" *> instseqP "]")

doblkP :: Parser Inst
doblkP = fmap Doblk
    (strP "do" *> whiteP *> instseqP "end")

nameblkP :: Parser Inst
nameblkP = fmap Nameblk
    (strP "block" *> whiteP *> identStrP <* whiteP)
    <*> instseqP "end"

typblkP :: Parser Inst
typblkP = fmap Typblk
    (strP "do" *> whiteP *> typeP)
    <*> instseqP "end"

nametypblkP :: Parser Inst
nametypblkP = fmap NameTypblk
    (strP "block" *> whiteP *> identStrP <* whiteP)
    <*> (typeP <* whiteP)
    <*> instseqP "end"

instseqP :: String -> Parser [Inst]
instseqP end = strP end *> pure [] <|>
    fmap (:) (instP <* whiteP) <*> instseqP end

identifierP :: Parser Inst
identifierP = fmap Identifier identStrP

identStrP :: Parser String
identStrP = charP '{' *> spanP (/='}') <* charP '}'
    <|> wordP

commentP :: Parser [String]
commentP = many ( whiteP
    *> strP "//" *> spanP (/='\n')
    <* optP (charP '\n')
    <* whiteP)

errP :: Parser a
errP = Parser $
    \ input -> Parsed input $ Left $ (err input)
    where err = Utils.fork str' loc word
          str' l s = show l ++ ": Unknown word found: `" ++ s ++ "`"
          word = either undefined id . value . runP wordP

i64 :: TypeSig
i64 = Tconst I64

builtinTyp :: Builtin -> TypeSig
builtinTyp b = case b of
    Add     -> Tfunc [ i64   , i64    ] [ i64            ]
    Sub     -> Tfunc [ i64   , i64    ] [ i64            ]
    Swap    -> Tfunc [ Tvar 0, Tvar 1 ] [ Tvar 1, Tvar 0 ]
    Dup     -> Tfunc [ Tvar 0         ] [ Tvar 0, Tvar 0 ]
    Drop    -> Tfunc [ Tvar 0         ] [                ]
    Print   -> Tfunc [ Tvar 0         ] [                ]
    Apply   -> Tfunc [ Tfunc [Tvar 0] [Tvar 1], Tvar 0 ] [ Tvar 1         ]
    Halt    -> Tfunc [                ] [                ]

instTyp :: Inst -> TypeSig
instTyp i = case i of
    Push    _ -> i64
    Builtin b -> builtinTyp b
    PQuote  _ -> undefined
    Doblk   _ -> undefined
    Nameblk _ _ -> undefined
    Typblk  _ _ -> undefined
    NameTypblk _ _ _ -> undefined
    Identifier _ -> undefined
