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

tpp :: Block -> String
tpp (Block inp out _) = "[ " ++
    (if inp == [] then "" else show inp) ++ " -- " ++
    (if out == [] then "" else show out) ++ " ]"

instance Show Block where
    show b@(Block inp out inst)
        = "do "++ tpp b ++ " " ++
        foldr (\ i s -> show i ++ " " ++ s) "" inst ++ "end"

data TypeSig
    = I64
    deriving (Show, Eq)

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
    | Doblk [Inst]
    | Blk Block

instance Show Inst where
    show (Push    x) = show x
    show (Swap     ) = "~"
    show (Dup      ) = ":"
    show (Drop     ) = ","
    show (Print    ) = "print"
    show (Halt     ) = "halt"
    show (Builtin b) = show b
    show (Doblk  is) = "do "++
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
    <|> doblkP
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

doblkP :: Parser Inst
doblkP = fmap Doblk
    (strP "do" *> whiteP *> some (instP <* whiteP) <* strP "end")

-- blockP :: Parser Block
-- blockP = Block [] [] <$>
--     (strP "do" *> whiteP *> some (instP <* whiteP) <* strP "end")

errP :: Parser Inst
errP = Parser $
    \ input -> Parsed input $ Left $ (err input)
    where err = Utils.fork str' loc word
          str' l s = show l ++ ": Unknown word found: `" ++ s ++ "`"
          word = either undefined id . value . runP wordP

instTyp :: Inst -> ([TypeSig], [TypeSig])
instTyp i = case i of
    Push    x -> ([]         , [I64]     )
    Swap      -> ([I64, I64] , [I64, I64])
    Dup       -> ([I64]      , [I64, I64])
    Drop      -> ([I64]      , []        )
    Print     -> ([I64]      , []        )
    Halt      -> ([]         , []        )
    Builtin b -> ([I64, I64] , [I64]     )
    Doblk   b -> undefined
    Blk     b -> (inpT b     , outT b    )
