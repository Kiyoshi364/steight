module Parsing.Parser
    ( parse
    ) where

import IR.Identifier (Identifier(..), mk_normal, mk_type
    , mk_constructor, mk_destructor, mk_builtin)
import qualified IR.Identifier as Id (Normal, Type
    , Constructor, Destructor, Builtin)
import IR.Token (Name(..), Tkn(..), Loc, Token(..)
    , fromName, emptyLoc, assertLocMerge, assertLocSkip, ppTokens)
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
                -> (l, (INormal n, (ASTBlock    l m_l_typ is)))
            TypeDecl    (_, n)    l_typ cs
                -> (l, (IType   n, (ASTTypeDecl l   l_typ cs)))
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

matchAnyName :: [Name] -> Parser (Loc, Name)
matchAnyName = fmap (TkName \\ tk)
    \\ matchAnyP
    \\ fmap (fork (,) loc (tkn \\ getName))

getName :: Tkn -> Name
getName (TkName n) = n
getName t = error $ "Parsing.Parser.getName: expected TkName " ++
    "found `" ++ show t ++ "`"

getComment :: Tkn -> String
getComment (TkComment s) = s
getComment t = error $ "Parsing.Parser.getComment: expected TkComment " ++
    "found `" ++ show t ++ "`"

getNum :: Tkn -> Int
getNum (TkName n) = case n of
    NTvar   num -> parseNum num
    NTmany  num -> parseNum num
    NNumber num -> parseNum num
    t           -> error $ "Parsing.Parser.getNum: expected " ++
        "NTvar, NTmany or NNumber found `" ++ show t ++ "`"
getNum t = error $ "Parsing.Parser.getNum: expected TkName " ++
    "found `" ++ show t ++ "`"

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
        |$> fork Inst loc (tkn \\ getNum \\ Push)

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
        <$> fmap Just idNormalP
        <*> optP typeLitP
        <*> instendp instP TkEnd)

helpBlock :: Maybe (Loc, Id.Normal) -> Maybe (Loc, TypeLit)
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
        <$> idTypeP
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
        <$> idNormalP
        <*> typeLitP)
  where
    helpCase :: (Loc, Id.Normal) -> (Loc, TypeLit) -> (Loc, CaseDecl)
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
identifierP = fmap (uncurry Inst . onSnd Identifier)
    (   onSnd INormal      <$> idNormalP
    <|> onSnd IType        <$> idTypeP
    <|> onSnd IConstructor <$> idConstructorP
    <|> onSnd IDestructor  <$> idDestructorP
    <|> onSnd IBuiltin     <$> idBuiltinP
    )

idNormalP :: Parser (Loc, Id.Normal)
idNormalP = onSnd (fromName \\ mk_normal)
    <$> matchAnyName [ NDown "", NSymbol "" ]

idTypeP :: Parser (Loc, Id.Type)
idTypeP = idMacroP mk_type $ NUp ""

idConstructorP :: Parser (Loc, Id.Constructor)
idConstructorP = idMacroP mk_constructor $ NIntro ""

idDestructorP :: Parser (Loc, Id.Destructor)
idDestructorP = idMacroP mk_destructor $ NElim ""

idBuiltinP :: Parser (Loc, Id.Builtin)
idBuiltinP = idMacroP mk_builtin $ NBuiltin ""

idMacroP :: (String -> a) -> Name -> Parser (Loc, a)
idMacroP ctor name = onSnd (fromName \\ ctor)
    <$> matchAnyName [ name ]

commentP :: Parser (Loc, String)
commentP = fmap (fork (,) loc $ getComment . tkn) $ matchTk $ TkComment ""

commentsLocMerge :: [(Loc, String)] -> (Loc, String)
commentsLocMerge = onSnd (maybe "" id) . foldr
    (\ (l1, c) (l2, ms) ->
        (assertLocMerge l1 l2, Just $ c ++ maybe "" ("\n" ++) ms))
    (emptyLoc, Nothing)

commentsLocSkip :: [(Loc, String)] -> Inst -> Inst
commentsLocSkip = commentsLocMerge \\ fst
    \\ (\ l1 -> fork Inst (assertLocSkip l1 . iloc) instr)

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
        <|> fork (,) loc (tkn \\ getNum \\ Avar \\ Left)
            <$> (matchTk (TkName $ NTvar ""))
        <|> fork (,) loc (tkn \\ getNum \\ Amany \\ Left)
            <$> (matchTk (TkName $ NTmany ""))
