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
    { dict :: Dict String [Inst]
    }

instance Show AST where show (AST ds) = "AST " ++ show ds

ipp :: Show a => [a] -> String
ipp = foldr (\ i s -> show i ++ " " ++ s) ""

data Builtin
    = Add
    | Sub
    deriving Eq

instance Show Builtin where
    show (Add   ) = "+"
    show (Sub   ) = "-"

data Inst
    = Push Int
    | Swap
    | Dup
    | Drop
    | Print
    | Halt
    | Builtin Builtin
    | Doblk [Inst]
    | Nameblk String [Inst]
    | Typblk TypeSig [Inst]
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Swap     ) = "~"
    show (Dup      ) = ":"
    show (Drop     ) = "."
    show (Print    ) = "print"
    show (Halt     ) = "halt"
    show (Builtin b) = show b
    show (Doblk  is) = "do " ++ ipp is ++ "end"
    show (Typblk typ is) = "do <" ++ show typ ++ "> " ++
        ipp is ++ "end"
    show (Nameblk name is) = "block " ++ name ++ " " ++ ipp is ++ "end"

lexer :: Parser AST
lexer = fmap AST $ some (
    fmap (\i -> case i of Nameblk s is -> (s, is); _ -> ("", [Halt]))
    (nameblkP <* whiteP)
    ) <* eofP

instP :: Parser Inst
instP = pushP
    <|> swapP <|> dupP <|> dropP
    <|> printP <|> haltP
    <|> builtinP
    <|> typblkP <|> doblkP
    <|> nameblkP
    <|> errP

pushP :: Parser Inst
pushP = fmap Push numP

swapP :: Parser Inst
swapP = (strP "swap" <|> strP "~") *> pure Swap

dupP  :: Parser Inst
dupP  = (strP "dup" <|> strP ":") *> pure Dup

dropP :: Parser Inst
dropP = (strP "drop" <|> strP ".") *> pure Drop

printP :: Parser Inst
printP = strP "print" *> pure Print

haltP :: Parser Inst
haltP = strP "<>" *> pure Halt

builtinP :: Parser Inst
builtinP = fmap Builtin $ addP <|> subP

addP :: Parser Builtin
addP = charP '+' *> pure Add

subP :: Parser Builtin
subP = charP '-' *> pure Sub

doblkP :: Parser Inst
doblkP = fmap Doblk
    (strP "do" *> whiteP *> instseqP <* strP "end")

nameblkP :: Parser Inst
nameblkP = fmap Nameblk
    (strP "block" *> whiteP *> wordP <* whiteP)
    <*> (instseqP <* strP "end")

typblkP :: Parser Inst
typblkP = fmap Typblk
    (strP "do" *> whiteP *> typeP)
    <*> (instseqP <* strP "end")

instseqP :: Parser [Inst]
instseqP = some $ instP <* whiteP

errP :: Parser Inst
errP = Parser $
    \ input -> Parsed input $ Left $ (err input)
    where err = Utils.fork str' loc word
          str' l s = show l ++ ": Unknown word found: `" ++ s ++ "`"
          word = either undefined id . value . runP wordP

i64 :: TypeSig
i64 = Tconst I64

builtinTyp :: Builtin -> TypeSig
builtinTyp b = case b of
    Add -> Tfunc [ i64, i64 ] [ i64 ]
    Sub -> Tfunc [ i64, i64 ] [ i64 ]

instTyp :: Inst -> TypeSig
instTyp i = case i of
    Push    _ -> Tconst                     I64
    Swap      -> Tfunc [ Tvar 0, Tvar 1 ] [ Tvar 1, Tvar 0 ]
    Dup       -> Tfunc [ Tvar 0         ] [ Tvar 0, Tvar 0 ]
    Drop      -> Tfunc [ Tvar 0         ] [                ]
    Print     -> Tfunc [ Tvar 0         ] [                ]
    Halt      -> Tfunc [                ] [                ]
    Builtin b -> builtinTyp b
    Doblk   _ -> undefined
    Nameblk _ _ -> undefined
    Typblk  _ _ -> undefined
