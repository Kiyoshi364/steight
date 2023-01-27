module Parsing.Parser
    ( parse
    ) where

import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , fromName, emptyLoc, assertLocMerge, ppTokens)
import IR.AST (AST(..), ASTEntry(..)
    , Builtin(..), AVar(..) , TypeLit(..)
    , CaseDecl(..), Inst(..), Instruction(..), cons, emptyAST)
import Parsing.Lexer (parseNum)
import Parsing.ParserLib
    ( ParserLib(..), (<|>) , failWithErrP
    , matchP, matchAnyP, optP, zeroOrMoreP, oneOrMoreP
    )
import Utils ((\\) , (|$>) , (...) , fork, onFst, onSnd, asList)

type Error = [] (Loc, String)
type Parser = ParserLib Error [] Token

parse :: [Token] -> Either Error AST
parse tokens = case runP parser tokens of
    ([], Right (_, ast)) -> Right ast
    ([], Left      err)  -> Left  err
    (ts, Right _      )  -> Left $ (:[])  $ mk_err ts
    (ts, Left      err)  -> Left $ (:err) $ mk_err ts
  where
    mk_err :: [] Token -> (Loc, String)
    mk_err ts = (,) (loc $ head ts) $
        "Parser error: non-exhaustive token-list: `" ++
        ppTokens ts ++ "`"

parser :: Parser (Loc, AST)
parser = oneOrMoreP (
        topLvlP
        |$> (\ (Inst l inst) -> case inst of
            -- Note: here the name location is dropped
            -- don't know where to use it
            Block (Just (_, n)) m_l_typ is
                -> (l, (n, (ASTBlock    l m_l_typ is)))
            TypeDecl    (_, n)    l_typ cs
                -> (l, (n, (ASTTypeDecl l   l_typ cs)))
            _ -> error $ "Parsing.parser: non-exhaustive pattern: " ++ show inst)
    )
    |$> asList \\ joinLocs \\ onSnd (fmap snd)
        \\ onSnd (foldr (uncurry cons) emptyAST)
        \\ (\ (l1, ast) l2 -> (assertLocMerge l1 l2, ast))
    <*> (
        (commentsLocMerge \\ fst \\ assertLocMerge)
        <$> zeroOrMoreP commentP
        <*> match TkEOF)

tk :: Tkn -> Token
tk = Tk emptyLoc

matchTk :: Tkn -> Parser Token
matchTk = matchP . tk

match :: Tkn -> Parser Loc
match = fmap loc . matchTk

-- Note: maybe it should return a Name
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
inTypeP = commentsLocSkip <$> zeroOrMoreP commentP <*>
    (   pushIntP
    <|> builtinP
    <|> quotedP
    <|> identifierP
    <|> fmap (fork Inst fst $ PType . snd) typeLitP
    <|> errP)

instP :: Parser (Loc, Inst)
instP = (fork (,) iloc id ... commentsLocSkip) <$> zeroOrMoreP commentP <*>
    ( inTypeP
    <|> doblkP <|> nameblkP
    <|> errP)

topLvlP :: Parser Inst
topLvlP = commentsLocSkip <$> zeroOrMoreP commentP <*>
    (nameblkP <|> typedeclP <|> errP)

pushIntP :: Parser Inst
pushIntP = matchTk (TkName $ NNumber "")
        |$> fork Inst loc (tkn \\ getString \\ parseNum \\ Push)

builtinP :: Parser Inst
builtinP = fmap (fork Inst fst $ Builtin . snd) $
        addP   <|> subP
    <|> swapP  <|> rotP <|> dupP  <|> dropP
    <|> printP <|> haltP
    <|> applyP

addP :: Parser (Loc, Builtin)
addP = (,) <$> match TkAdd <*> pure Add

subP :: Parser (Loc, Builtin)
subP = (,) <$> match TkSub <*> pure Sub

swapP :: Parser (Loc, Builtin)
swapP = (,) <$> match TkSwap <*> pure Swap

rotP :: Parser (Loc, Builtin)
rotP = (,) <$> match TkRot <*> pure Rot

dupP  :: Parser (Loc, Builtin)
dupP  = (,) <$> match TkDup <*> pure Dup

dropP :: Parser (Loc, Builtin)
dropP = (,) <$> match TkDrop <*> pure Drop

printP :: Parser (Loc, Builtin)
printP = (,) <$> match TkPrint <*> pure Print

haltP :: Parser (Loc, Builtin)
haltP = (,) <$> match TkHalt <*> pure Halt

applyP :: Parser (Loc, Builtin)
applyP = (,) <$> match TkApply <*> pure Apply

quotedP :: Parser Inst
quotedP = uncurry Inst . onSnd PQuote
    <$> instseqp instP TkOpenBrack TkCloseBrack

doblkP :: Parser Inst
doblkP = withLoc1 Inst
    <$> match TkDo
    <*> (helpBlock Nothing
        <$> optP typeLitP
        <*> instendp instP TkEnd)

nameblkP :: Parser Inst
nameblkP = withLoc1 Inst
    <$> match TkBlock
    <*> (helpBlock
        <$> fmap Just newIdP
        <*> optP typeLitP
        <*> instendp instP TkEnd)

helpBlock :: Maybe (Loc, String) -> Maybe (Loc, TypeLit)
    -> (Loc, [Inst]) -> (Loc, Instruction)
helpBlock m_name m_typ (l3, is) =
    let
        l1 = maybe emptyLoc fst m_name
        l2 = maybe emptyLoc fst m_typ
        l = foldr assertLocMerge emptyLoc [l1, l2, l3]
    in (l, Block m_name m_typ is)

typedeclP :: Parser Inst
typedeclP = withLoc1 Inst
    <$> match TkType
    <*> (helpType
        <$> newIdP
        <*> typeLitP
        <*> instructionendp casesP TkEnd)
  where
    helpType newid@(l1, _) typ@(l2, _) (l3, cases) =
        let l = foldr assertLocMerge emptyLoc [l1, l2, l3]
        in (l, TypeDecl newid typ cases)

casesP :: Parser (Loc, CaseDecl)
casesP = withLoc1 (,)
    <$> match TkCase
    <*> (helpCase
        <$> newIdP
        <*> typeLitP)
  where
    helpCase :: (Loc, String) -> (Loc, TypeLit) -> (Loc, CaseDecl)
    helpCase newid@(l1, _) typ@(l2, _) =
        (assertLocMerge l1 l2, CaseDecl newid typ)

withLoc1 :: (Loc -> a -> b) -> Loc -> (Loc, a) -> b
withLoc1 f l1 (l2, x) = f (assertLocMerge l1 l2) x

instructionseqp :: Parser (Loc, a) -> Tkn -> Tkn -> Parser (Loc, [(Loc, a)])
instructionseqp p start end = onFst . assertLocMerge
    <$> match start
    <*> instructionendp p end

instructionendp :: Parser (Loc, a) -> Tkn -> Parser (Loc, [(Loc, a)])
instructionendp p end = joinListLocsLoc
    <$> zeroOrMoreP p
    <*> match end
  where
    joinListLocsLoc :: [(Loc, a)] -> Loc -> (Loc, [(Loc, a)])
    joinListLocsLoc xs l =
        onFst (flip assertLocMerge l) $ joinLocs xs

instseqp :: Parser (Loc, Inst) -> Tkn -> Tkn -> Parser (Loc, [Inst])
instseqp p start end = instructionseqp p start end
    |$> onSnd (fmap snd)

instendp :: Parser (Loc, Inst) -> Tkn -> Parser (Loc, [Inst])
instendp p end = instructionendp p end
    |$> onSnd (fmap snd)

joinLocs :: [(Loc, a)] -> (Loc, [(Loc, a)])
joinLocs = foldr
    (\ x@(l1, _) (l2, xs) -> (assertLocMerge l1 l2, x:xs) )
    (emptyLoc, [])

identifierP :: Parser Inst
identifierP = fork Inst fst (snd \\ Identifier) <$> matchAnyName
    [ NUp      "" , NDown    "" , NIntro   ""
    , NElim    "" , NBuiltin "" , NSymbol  "" ]

newIdP :: Parser (Loc, String)
newIdP = matchAnyName [ NUp "" , NDown "" , NSymbol "" ]

commentP :: Parser (Loc, String)
commentP = fmap (fork (,) loc $ getString . tkn) $ matchTk $ TkComment ""

commentsLocMerge :: [(Loc, String)] -> (Loc, String)
commentsLocMerge = onSnd (maybe "" id) . foldr
    (\ (l1, c) (l2, ms) ->
        (assertLocMerge l1 l2, Just $ c ++ maybe "" ("\n" ++) ms))
    (emptyLoc, Nothing)

commentsLocSkip :: [(Loc, String)] -> Inst -> Inst
commentsLocSkip = commentsLocMerge \\ fst
    \\ (\ l1 -> fork Inst (assertLocMerge l1 . iloc) instr)
    -- \\ ( \ l1 (Inst l2 i) -> Inst (assertLocMerge l1 l2) i)

errP :: Parser a
errP = failWithErrP $ \ mt -> case mt of
    Just (Tk l tn) -> (:[]) $ (,) l $ "Unexpected token found: " ++ show tn
    Nothing        -> error "Parsing.Parser.errP: found end of tokens"

typeLitP :: Parser (Loc, TypeLit)
typeLitP = (\ (li,i) (lo,o) ->
    (assertLocMerge li lo, TypeLit i o)) <$> inpp <*> outp
  where
    inpp :: Parser (Loc, [(Loc, Either AVar Inst)])
    inpp = onSnd reverse <$> instructionseqp typp TkOpenPar TkDash
        <|> (,) <$> match TkOpenPar <*> pure []
    outp :: Parser (Loc, [(Loc, Either AVar Inst)])
    outp = onSnd reverse <$> instructionendp typp TkClosePar
    typp :: Parser (Loc, Either AVar Inst)
    typp = fork (,) iloc Right <$> inTypeP
        <|> (\ t e_ai -> (loc t, Inst (loc t) <$> e_ai))
            <$> matchTk TkI64b <*> pure (Right $ Builtin I64b)
        <|> fork (,) loc (tkn \\ getString \\ parseNum \\ Avar \\ Left)
            <$> (matchTk (TkName $ NTvar ""))
        <|> fork (,) loc (tkn \\ getString \\ parseNum \\ Amany \\ Left)
            <$> (matchTk (TkName $ NTmany ""))
