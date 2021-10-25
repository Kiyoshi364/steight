module Inst where

import Parser
import Control.Applicative
import qualified Utils

type Program = [(Int, Inst)]

data Builtin
    = Add
    | Sub

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

instance Show Inst where
    show (Push    x) = show x
    show (Swap     ) = "~"
    show (Dup      ) = ":"
    show (Drop     ) = ","
    show (Print    ) = "print"
    show (Halt     ) = "halt"
    show (Builtin b) = show b

lexer :: Parser Program
lexer = fmap (zip [0..]) $ untilEof $ many (wsP <|> lfP) *> instP

instP :: Parser Inst
instP = pushP
    <|> swapP <|> dupP <|> dropP
    <|> printP <|> haltP
    <|> builtinP
    <|> eofP *> pure Halt <|> errP

pushP :: Parser Inst
pushP = fmap Push numP

swapP :: Parser Inst
swapP = (strP "swap" <|> strP "~") *> pure Swap

dupP  :: Parser Inst
dupP  = (strP "dup" <|> strP ":") *> pure Dup

dropP :: Parser Inst
dropP = (strP "drop" <|> strP ",") *> pure Drop

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

errP :: Parser Inst
errP = Parser $
    \ input -> Parsed input $ Left $ (err input)
    where err = Utils.fork str' loc word
          str' l s = show l ++ ": Unknown word found: `" ++ s ++ "`"
          word = either undefined id . value . runP wordP
