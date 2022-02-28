module Parsing.Parser
    ( parse
    ) where

import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , fromName, emptyLoc, ppTokens)
import IR.AST (AST(..), Builtin(..), TypeLit(..), Inst(..), cons, emptyAST)
import Parsing.Lexer (parseNum)
import Parsing.ParserLib
    ( ParserLib(..), (<|>) , failWithErrP
    , matchP, matchAnyP, zeroOrMoreP, oneOrMoreP
    )
import Utils ((\\) , (|$>) , asList)

type Error = [] (Loc, String)
type Parser = ParserLib Error [] Token

parse :: [Token] -> Either Error AST
parse tokens = case runP parser tokens of
    ([], Right ast) -> Right ast
    ([], Left  err) -> Left  err
    (ts, Right _  ) -> Left $ (:[])  $ mk_err ts
    (ts, Left  err) -> Left $ (:err) $ mk_err ts
  where
    mk_err :: [] Token -> (Loc, String)
    mk_err ts = (,) (loc $ head ts) $
        "Parser error: non-exhaustive token-list: `" ++
        ppTokens ts ++ "`"

parser :: Parser AST
parser = oneOrMoreP (
        topLvlP
        |$> (\i -> case i of
            Nameblk    s     is -> (s, (Nothing , is))
            NameTypblk s typ is -> (s, (Just typ, is))
            _ -> error "Inst.lexer: non-exhaustive pattern")
        ) <* commentP <* match TkEOF
        |$> asList \\ foldr (uncurry cons) emptyAST

tk :: Tkn -> Token
tk = Tk emptyLoc

match :: Tkn -> Parser Token
match = matchP . tk

matchAnyName :: [Name] -> Parser String
matchAnyName = fmap (TkName \\ tk)
    \\ matchAnyP
    \\ fmap (tkn \\ getString)

getString :: Tkn -> String
getString (TkName n) = fromName n
getString (TkComment s) = s
getString t = error $ "Parsing.Parser.getString: expected TkName or " ++
    "TkComment found `" ++ show t ++ "`"

inTypeP :: Parser Inst
inTypeP = commentP *>
    (   pushIntP
    <|> builtinP
    <|> quotedP
    <|> identifierP
    <|> fmap PType typeLitP
    <|> errP)

instP :: Parser Inst
instP = commentP *>
    ( inTypeP
    <|> typblkP <|> doblkP
    <|> nametypblkP <|> nameblkP
    <|> errP)

topLvlP :: Parser Inst
topLvlP = commentP *>
    ( nametypblkP <|> nameblkP
    <|> errP)

pushIntP :: Parser Inst
pushIntP = matchP (tk $ TkName $ NNumber "")
        |$> (tkn \\ getString \\ parseNum \\ Push)

builtinP :: Parser Inst
builtinP = fmap Builtin $
        addP   <|> subP
    <|> swapP  <|> rotP <|> dupP  <|> dropP
    <|> printP <|> haltP
    <|> applyP

addP :: Parser Builtin
addP = match TkAdd *> pure Add

subP :: Parser Builtin
subP = match TkSub *> pure Sub

swapP :: Parser Builtin
swapP = match TkSwap *> pure Swap

rotP :: Parser Builtin
rotP = match TkRot *> pure Rot

dupP  :: Parser Builtin
dupP  = match TkDup *> pure Dup

dropP :: Parser Builtin
dropP = match TkDrop *> pure Drop

printP :: Parser Builtin
printP = match TkPrint *> pure Print

haltP :: Parser Builtin
haltP = match TkHalt *> pure Halt

applyP :: Parser Builtin
applyP = match TkApply *> pure Apply

quotedP :: Parser Inst
quotedP = PQuote <$> instseqp instP TkOpenBrack TkCloseBrack

doblkP :: Parser Inst
doblkP = Doblk <$> instseqp instP TkDo TkEnd

nameblkP :: Parser Inst
nameblkP = Nameblk <$> (match TkBlock *> newIdP) <*> instendp instP TkEnd

typblkP :: Parser Inst
typblkP = Typblk <$> (match TkDo *> typeLitP) <*> instendp instP TkEnd

nametypblkP :: Parser Inst
nametypblkP = NameTypblk
    <$> (match TkBlock *> newIdP) <*> typeLitP <*> instendp instP TkEnd

instseqp :: Parser a -> Tkn -> Tkn -> Parser [a]
instseqp p start end = match start *> instendp p end

instendp :: Parser a -> Tkn -> Parser [a]
instendp p end = zeroOrMoreP p <* match end

identifierP :: Parser Inst
identifierP = Identifier <$> matchAnyName
    [ NUp      "" , NDown    "" , NIntro   ""
    , NElim    "" , NBuiltin "" , NSymbol  "" ]

newIdP :: Parser String
newIdP = matchAnyName [ NUp "" , NDown "" , NSymbol "" ]

commentP :: Parser [String]
commentP = zeroOrMoreP $ fmap (getString . tkn) $ matchP $ tk $ TkComment ""

errP :: Parser a
errP = failWithErrP $ \ mt -> case mt of
    Just (Tk l tn) -> (:[]) $ (,) l $ "Unexpected token found:" ++ show tn
    Nothing        -> error "Parsing.Parser.errP: found end of tokens"

typeLitP :: Parser TypeLit
typeLitP = TypeLit . reverse <$> inpp <*> fmap reverse outp
  where
    inpp :: Parser [Inst]
    inpp = instseqp inTypeP TkOpenPar TkDash
        <|> match TkOpenPar *> pure []
    outp :: Parser [Inst]
    outp = instendp inTypeP TkClosePar
