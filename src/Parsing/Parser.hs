module Parsing.Parser
    ( parse
    ) where

import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , fromName, emptyLoc, ppTokens)
import IR.AST (AST(..), Builtin(..), AVar(..), TypeLit(..)
    , Inst, Instruction(..), cons, emptyAST)
import Parsing.Lexer (parseNum)
import Parsing.ParserLib
    ( ParserLib(..), (<|>) , failWithErrP
    , matchP, matchAnyP, optP, zeroOrMoreP, oneOrMoreP
    )
import Utils ((\\) , (|$>) , (\\\) , fork, onSnd, asList)

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
        |$> (\ i -> case i of
            (l, Block (Just s) m_typ is) -> (s, (l, m_typ, is))
            _ -> error "Parsing.parser: non-exhaustive pattern")
        ) <* zeroOrMoreP commentP <* match TkEOF
        |$> asList \\ foldr (uncurry cons) emptyAST

tk :: Tkn -> Token
tk = Tk emptyLoc

match :: Tkn -> Parser Token
match = matchP . tk

matchAnyName :: [Name] -> Parser (Loc, String)
matchAnyName = fmap (TkName \\ tk)
    \\ matchAnyP
    \\ fmap (fork (,) loc (tkn \\ getString))

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
    <|> fmap (onSnd PType) typeLitP
    <|> errP)

instP :: Parser Inst
instP = zeroOrMoreP commentP *>
    ( inTypeP
    <|> doblkP <|> nameblkP
    <|> errP)

topLvlP :: Parser Inst
topLvlP = zeroOrMoreP commentP *> (nameblkP <|> errP)

pushIntP :: Parser Inst
pushIntP = match (TkName $ NNumber "")
        |$> fork (,) loc (tkn \\ getString \\ parseNum \\ Push)

builtinP :: Parser Inst
builtinP = fmap (onSnd Builtin) $
        addP   <|> subP
    <|> swapP  <|> rotP <|> dupP  <|> dropP
    <|> printP <|> haltP
    <|> applyP

addP :: Parser (Loc, Builtin)
addP = (,) . loc <$> match TkAdd <*> pure Add

subP :: Parser (Loc, Builtin)
subP = (,) . loc <$> match TkSub <*> pure Sub

swapP :: Parser (Loc, Builtin)
swapP = (,) . loc <$> match TkSwap <*> pure Swap

rotP :: Parser (Loc, Builtin)
rotP = (,) . loc <$> match TkRot <*> pure Rot

dupP  :: Parser (Loc, Builtin)
dupP  = (,) . loc <$> match TkDup <*> pure Dup

dropP :: Parser (Loc, Builtin)
dropP = (,) . loc <$> match TkDrop <*> pure Drop

printP :: Parser (Loc, Builtin)
printP = (,) . loc <$> match TkPrint <*> pure Print

haltP :: Parser (Loc, Builtin)
haltP = (,) . loc <$> match TkHalt <*> pure Halt

applyP :: Parser (Loc, Builtin)
applyP = (,) . loc <$> match TkApply <*> pure Apply

quotedP :: Parser Inst
quotedP = onSnd PQuote <$> instseqp instP TkOpenBrack TkCloseBrack

doblkP :: Parser Inst
doblkP = (,) . loc
    <$> match TkDo
    <*> (Block Nothing
        <$> optP (fmap snd typeLitP)
        <*> instructionendp instP TkEnd)

nameblkP :: Parser Inst
nameblkP = (,) . loc
    <$> match TkBlock
    <*> (Block
        <$> fmap (Just . snd) newIdP
        <*> optP (fmap snd typeLitP)
        <*> instructionendp instP TkEnd)

instructionseqp :: Parser a -> Tkn -> Tkn -> Parser [a]
instructionseqp p start end = match start *> instructionendp p end

instructionendp :: Parser a -> Tkn -> Parser [a]
instructionendp p end = zeroOrMoreP p <* match end

instseqp :: Parser (Loc, a) -> Tkn -> Tkn -> Parser (Loc, [(Loc, a)])
instseqp p start end = instructionseqp p start end
    |$> foldr (\ x@(l,_) (_,xs) -> (l, x:xs) ) (emptyLoc, [])

identifierP :: Parser Inst
identifierP = onSnd Identifier <$> matchAnyName
    [ NUp      "" , NDown    "" , NIntro   ""
    , NElim    "" , NBuiltin "" , NSymbol  "" ]

newIdP :: Parser (Loc, String)
newIdP = matchAnyName [ NUp "" , NDown "" , NSymbol "" ]

commentP :: Parser (Loc, String)
commentP = fmap (fork (,) loc $ getString . tkn) $ match $ TkComment ""

errP :: Parser a
errP = failWithErrP $ \ mt -> case mt of
    Just (Tk l tn) -> (:[]) $ (,) l $ "Unexpected token found:" ++ show tn
    Nothing        -> error "Parsing.Parser.errP: found end of tokens"

typeLitP :: Parser (Loc, TypeLit)
typeLitP = (\ (l,i) o -> (l, TypeLit i o)) <$> inpp <*> outp
  where
    inpp :: Parser (Loc, [Either AVar Inst])
    inpp = onSnd (map snd \\ reverse) <$> instseqp typp TkOpenPar TkDash
        <|> (,) . loc <$> match TkOpenPar <*> pure []
    outp :: Parser [Either AVar Inst]
    outp = map snd \\ reverse <$> instructionendp typp TkClosePar
    typp :: Parser (Loc, Either AVar Inst)
    typp = fork (,) fst Right <$> inTypeP
        <|> (,) \\\ uncurry (\ t e_ai -> (loc t, (,) (loc t) <$> e_ai))
            <$> match TkI64b <*> pure (Right $ Builtin I64b)
        <|> fork (,) loc (tkn \\ getString \\ parseNum \\ Avar \\ Left)
            <$> (match (TkName $ NTvar ""))
        <|> fork (,) loc (tkn \\ getString \\ parseNum \\ Amany \\ Left)
            <$> (match (TkName $ NTmany ""))
