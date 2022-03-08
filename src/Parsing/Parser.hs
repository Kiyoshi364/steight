module Parsing.Parser
    ( parse
    ) where

import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , fromName, emptyLoc, ppTokens)
import IR.AST (AST(..), Builtin(..), AVar(..), TypeLit(..), Inst(..)
    , cons, emptyAST)
import Parsing.Lexer (parseNum)
import Parsing.ParserLib
    ( ParserLib(..), (<|>) , failWithErrP
    , matchP, matchAnyP, optP, zeroOrMoreP, oneOrMoreP
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
            Block (Just s) m_typ  is -> (s, (m_typ, is))
            _ -> error "Parsing.parser: non-exhaustive pattern")
        ) <* zeroOrMoreP commentP <* match TkEOF
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
inTypeP = zeroOrMoreP commentP *>
    (   pushIntP
    <|> builtinP
    <|> quotedP
    <|> identifierP
    <|> fmap PType typeLitP
    <|> errP)

instP :: Parser Inst
instP = zeroOrMoreP commentP *>
    ( inTypeP
    <|> doblkP <|> nameblkP
    <|> errP)

topLvlP :: Parser Inst
topLvlP = zeroOrMoreP commentP *>
    ( nameblkP <|> errP)

pushIntP :: Parser Inst
pushIntP = match (TkName $ NNumber "")
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
doblkP = Block Nothing
    <$> (match TkDo *> optP typeLitP)
    <*> instendp instP TkEnd

nameblkP :: Parser Inst
nameblkP = Block
    <$> (match TkBlock *> fmap Just newIdP)
    <*> optP typeLitP
    <*> instendp instP TkEnd

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

commentP :: Parser String
commentP = fmap (getString . tkn) $ match $ TkComment ""

errP :: Parser a
errP = failWithErrP $ \ mt -> case mt of
    Just (Tk l tn) -> (:[]) $ (,) l $ "Unexpected token found:" ++ show tn
    Nothing        -> error "Parsing.Parser.errP: found end of tokens"

typeLitP :: Parser TypeLit
typeLitP = TypeLit <$> inpp <*> outp
  where
    inpp :: Parser [Either AVar Inst]
    inpp = reverse <$> instseqp typp TkOpenPar TkDash
        <|> match TkOpenPar *> pure []
    outp :: Parser [Either AVar Inst]
    outp = reverse <$> instendp typp TkClosePar
    typp :: Parser (Either AVar Inst)
    typp = Right <$> inTypeP
        <|> match TkI64b *> pure (Right $ Builtin I64b)
        <|> tkn \\ getString \\ parseNum \\ Avar \\ Left
            <$> (match (TkName $ NTvar ""))
        <|> tkn \\ getString \\ parseNum \\ Amany \\ Left
            <$> (match (TkName $ NTmany ""))
