module Inst where

import Parser
import Control.Applicative

type Program = [(Int, Inst)]

data Builtin
    = Add
    | Sub

instance Show Builtin where
    show (Add   ) = "+"
    show (Sub   ) = "-"

data Inst
    = Push Int
    | Halt
    | Builtin Builtin

instance Show Inst where
    show (Push    x) = show x
    show (Halt     ) = "hlt"
    show (Builtin b) = show b

instP :: Parser Inst
instP = pushP <|> haltP <|> builtinP

pushP :: Parser Inst
pushP = fmap Push numP

haltP :: Parser Inst
haltP = strP "<>" *> pure Halt

builtinP :: Parser Inst
builtinP = fmap Builtin $ addP <|> subP

addP :: Parser Builtin
addP = charP '+' *> pure Add

subP :: Parser Builtin
subP = charP '-' *> pure Sub
