module Inst where

import Parser
import Control.Applicative
import qualified Utils

data AST = AST
    { dict :: [(String, [Inst])]
    }
    deriving Show

tpp :: ([TypeSig], [TypeSig]) -> String
tpp ([]  , [] ) = ""
tpp (inp , out) = "[ " ++
    (if inp == [] then "" else revcat inp) ++ "-- " ++
    (if out == [] then "" else revcat out) ++ "]"
    where revcat  []    = ""
          revcat (x:xs) = foldl (\s t -> show t ++ ", " ++ s) (show x ++ " ") xs

tpp' :: ([TypeSig], [TypeSig]) -> String
tpp' ([]  , [] ) = "[ -- ]"
tpp' (inp , out) = tpp (inp, out)

ipp :: Show a => [a] -> String
ipp = foldr (\ i s -> show i ++ " " ++ s) ""

data TypeSig
    = I64
    deriving (Show, Eq)

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
    | Typblk [TypeSig] [TypeSig] [Inst]
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
    show (Typblk inp out is) = "do <" ++ tpp (inp, out) ++ "> " ++
        ipp is ++ "end"
    show (Nameblk name is) = "block " ++ name ++ " " ++ ipp is ++ "end"

lexer :: Parser AST
lexer = fmap AST $ some $
    fmap (\i -> case i of Nameblk s is -> (s, is); _ -> ("", [Halt]))
    (nameblkP <* whiteP)

instP :: Parser Inst
instP = pushP
    <|> swapP <|> dupP <|> dropP
    <|> printP <|> haltP
    <|> builtinP
    <|> typblkP <|> doblkP
    <|> nameblkP
    <|> eofP *> pure Halt <|> errP

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
typblkP = fmap (uncurry Typblk)
    (strP "do" *> whiteP *> btypP)
    <*> (instseqP <* strP "end")

instseqP :: Parser [Inst]
instseqP = some $ instP <* whiteP

btypP :: Parser ([TypeSig], [TypeSig])
btypP = fmap (\a b -> (a, b))
    (charP '[' *> whiteP *> sttypP <* strP "--")
    <*> (whiteP *> sttypP <* whiteP <* charP ']' <* whiteP)

sttypP :: Parser [TypeSig]
sttypP = many (typP <* whiteP)

typP :: Parser TypeSig
typP = strP "I64" *> pure I64

errP :: Parser Inst
errP = Parser $
    \ input -> Parsed input $ Left $ (err input)
    where err = Utils.fork str' loc word
          str' l s = show l ++ ": Unknown word found: `" ++ s ++ "`"
          word = either undefined id . value . runP wordP

builtinTyp :: Builtin -> ([TypeSig], [TypeSig])
builtinTyp b = case b of
    Add -> ([I64, I64] , [I64])
    Sub -> ([I64, I64] , [I64])

instTyp :: Inst -> ([TypeSig], [TypeSig])
instTyp i = case i of
    Push    x -> ([]         , [I64]     )
    Swap      -> ([I64, I64] , [I64, I64])
    Dup       -> ([I64]      , [I64, I64])
    Drop      -> ([I64]      , []        )
    Print     -> ([I64]      , []        )
    Halt      -> ([]         , []        )
    Builtin b -> builtinTyp b
    Doblk   b -> undefined
    Nameblk s b -> undefined
    Typblk i o s -> undefined
