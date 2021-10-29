module Inst where

import Parser
import Control.Applicative
import qualified Utils

data Program = Program
    { code :: [Inst]
    -- { main :: String
    -- , dict :: [(String, Block)]
    }
    deriving Show

data Block = Block
    { inpT  :: [TypeSig]
    , outT  :: [TypeSig]
    , insts :: [Inst]
    }
    deriving Eq

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

instance Show Block where
    show (Block inp out inst)
        = "do " ++ tpp (inp, out) ++ " " ++
        foldr (\ i s -> show i ++ " " ++ s) "" inst ++ "end"

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
    | Typblk [TypeSig] [TypeSig] [Inst]
    | Blk Block
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Swap     ) = "~"
    show (Dup      ) = ":"
    show (Drop     ) = "."
    show (Print    ) = "print"
    show (Halt     ) = "halt"
    show (Builtin b) = show b
    show (Doblk  is) = "do " ++
        foldr (\ i s -> show i ++ " " ++ s) "" is ++ "end"
    show (Typblk inp out is) = "do <" ++ tpp (inp, out) ++ "> " ++
        foldr (\ i s -> show i ++ " " ++ s) "" is ++ "end"
    show (Blk     b) = show b

lexer :: Parser [Inst]
lexer = fmap (\i -> case i of Doblk is -> is; _ -> []) doblkP
-- lexer = fmap (zip [0..]) $ untilEof $ many (wsP <|> lfP) *> instP

instP :: Parser Inst
instP = pushP
    <|> swapP <|> dupP <|> dropP
    <|> printP <|> haltP
    <|> builtinP
    <|> typblkP <|> doblkP
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
    (strP "do" *> whiteP *> some (instP <* whiteP) <* strP "end")

typblkP :: Parser Inst
typblkP = fmap (uncurry Typblk)
    (strP "do" *> whiteP *> btypP)
    <*> (some (instP <* whiteP) <* strP "end")

btypP :: Parser ([TypeSig], [TypeSig])
btypP = fmap (\a b -> (a, b))
    (charP '[' *> whiteP *> sttypP <* strP "--")
    <*> (whiteP *> sttypP <* whiteP <* charP ']' <* whiteP)

sttypP :: Parser [TypeSig]
sttypP = many (typP <* whiteP)

typP :: Parser TypeSig
typP = strP "I64" *> pure I64

-- blockP :: Parser Block
-- blockP = Block [] [] <$>
--     (strP "do" *> whiteP *> some (instP <* whiteP) <* strP "end")

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
    Typblk i o s -> undefined
    Blk     b -> (inpT b     , outT b    )
