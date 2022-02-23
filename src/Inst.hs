module Inst
    ( AST(..)
    , Builtin(..)
    , Inst(..)
    , ipp
    , instTyp
    , lexer
    ) where

import IR.AST
import Parser
import Control.Applicative
import Types (TypeSig(..), ConstT(..))
import Types.Parsing (typeP)
import qualified Utils

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
    <|> swapP  <|> rotP <|> dupP  <|> dropP
    <|> printP <|> haltP
    <|> applyP

addP :: Parser Builtin
addP = charP '+' *> pure Add

subP :: Parser Builtin
subP = charP '-' *> pure Sub

swapP :: Parser Builtin
swapP = (strP "swap" <|> strP "~") *> pure Swap

rotP :: Parser Builtin
rotP = strP "rot" *> pure Rot

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
    (strP "[" *> instseqP "]")

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

tmany :: Int -> TypeSig
tmany i = Tmany (i, 0)

builtinTyp :: Builtin -> TypeSig
builtinTyp b = case b of
    Add     -> Tfunc [ i64   , i64            ] [ i64                    ]
    Sub     -> Tfunc [ i64   , i64            ] [ i64                    ]
    Swap    -> Tfunc [ Tvar 0, Tvar 1         ] [ Tvar 1, Tvar 0         ]
    Rot     -> Tfunc [ Tvar 0, Tvar 1, Tvar 2 ] [ Tvar 2, Tvar 0, Tvar 1 ]
    Dup     -> Tfunc [ Tvar 0                 ] [ Tvar 0, Tvar 0         ]
    Drop    -> Tfunc [ Tvar 0                 ] [                        ]
    Print   -> Tfunc [ Tvar 0                 ] [                        ]
    Apply   -> Tfunc [ Tfunc [tmany 0] [tmany 1], tmany 0 ] [ tmany 1    ]
    Halt    -> Tfunc [                        ] [                        ]

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
