module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), ConstT(..), compose)
import IR.AST as AST (AST(..), Inst(..), TypeLit(..), builtinTyp)
import IR.Bytecode as Bcode
    (Bytecode(..), cons, Chunk(..), emptyChunk, ByteInst)
import qualified IR.Bytecode as ST (StkTyp(..))
import qualified IR.Bytecode as Bcode (ByteInst(..))
import Utils (assert, assertWith, loop)
import Dict (Dict, insert, find, partPair)

type ASTDict  = Dict String (Maybe TypeLit, [Inst])
type ByteDict  = Dict String Chunk

typecheckIO :: AST -> IO (Bytecode, Bool)
typecheckIO ast = do
    (err_, prog, []) <- return (typecheck ast)
    (mainTypOk, errs) <- return $ case find "main" prog of
        Nothing              -> (False, err_ ++
            ["main block not found or with an error"])
        Just (Chunk typ _ _) -> if typ == Tfunc [] [] then (True, err_)
            else (,) False $
            ("In main: main shoud have type `" ++
            show (Tfunc [] []) ++ "` but has type `" ++
            show typ ++ "`") : err_
    if length errs > 0
    then putStrLn "=== Errors: ==="
        >> mapM putStrLn (map (++"\n") errs)
        >> putStrLn "===============\n"
        >> if isOk "main" prog
            then return (Bytecode prog, mainTypOk)
            else return (Bytecode prog, False    )
    else return (Bytecode prog, mainTypOk)

isOk :: String -> ByteDict -> Bool
isOk str prog = case find str prog of
    Nothing               -> False
    Just (Chunk _ is scp) -> rec is (Bcode.dict scp) prog
  where
    rec  []     _  _ = True
    rec (i:is) scp p = case i of
        Bcode.ChkCall s -> rec is scp p && (isOk s p || isOk s scp)
        _               -> rec is scp p

typecheck :: AST -> ([String], ByteDict, ASTDict)
typecheck ast = fst $ loop iter ([], [], AST.dict ast)

iter :: ([String], ByteDict, ASTDict)
    -> Either () ([String], ByteDict, ASTDict)
iter (   _,    _,  []          ) = Left ()
iter (errs, prog, (str, (m_tl, is)):ast) = Right $
    let (errs', prog', ast', m_tp) = maybe (errs, prog, ast, Nothing) (
            \ tlit -> case typetypelit prog ast tlit of
                Left er          -> (er:errs, prog, ast, Nothing)
                Right (p, a, tp) -> (   errs, p   , a  , Just tp)
            )
            m_tl
            -- (flip (,) Nothing . (:[])) ((,) [] . Just)) m_tl
    in
    case typechunk prog ast str m_tp is of
        Left  err    -> (err:errs', prog', ast')
        Right (p, a) -> (    errs', p    , a   )

typetypelit :: ByteDict -> ASTDict -> TypeLit
    -> Either String (ByteDict, ASTDict, TypeSig)
typetypelit p a (TypeLit i o) = case fromInst p a (Doblk i) of
    Left err                          -> Left err
    Right (Tfunc typ_i [], p', a', _) ->
        case fromInst p' a' (Doblk o) of
            Left err                            -> Left err
            Right (Tfunc typ_o [], p'', a'', _) ->
                Right (p'', a'', Tfunc typ_i typ_o)
            x -> error $ "Typecheck.typetypelit: unhandled match case: "
                ++ show x
    x -> error $ "Typecheck.typetypelit: unhandled match case: "
        ++ show x

typechunk :: ByteDict -> ASTDict -> String -> Maybe TypeSig ->
    [Inst] -> Either String (ByteDict, ASTDict)
typechunk prog ast str m_tp is =
    assertWith (maybe True (const False) . find str)
        (\a -> "Found two blocks with name `" ++ str ++ "`\n\t" ++ show a)
        ast
    >> do_typechunk prog ast str m_tp emptyChunk is

do_typechunk :: ByteDict -> ASTDict -> String -> Maybe TypeSig ->
    Chunk -> [Inst] -> Either String (ByteDict, ASTDict)
do_typechunk prog ast str m_tp (Chunk stk by_is scp)  []    =
    case m_tp of
        Just tp -> assert tp (\ typ ->
                "The named-typed-block '" ++ str ++
                "' expected type was `" ++
                show tp ++ "` but actual type is `" ++
                show typ ++ "`") stk
            >> Right
            (insert str (Chunk stk (reverse by_is) scp) prog, ast)
        Nothing -> Right
            (insert str (Chunk stk (reverse by_is) scp) prog, ast)
do_typechunk prog ast str m_tp (Chunk stk by_is scp) (i:is) =
    case fromInst prog ast i of
        Left  err -> Left $ "In " ++ str ++ ": " ++ err
        Right (typ, p, a, e_sp_by_i) -> case e_sp_by_i of
            Left  s       -> do_typechunk p a str m_tp
                    (Chunk stk      by_is  (Bcode.cons s scp)) is
            Right by_i -> case compose stk typ of
                Right st  -> do_typechunk p a str m_tp
                    (Chunk st (by_i:by_is)               scp ) is
                Left  err ->
                    Left $ "In " ++ str ++
                    ": Instruction `" ++ show i ++ "`: " ++ err

fromInst :: ByteDict -> ASTDict -> Inst
    -> Either String
        (TypeSig, ByteDict, ASTDict, Either (String, Chunk) ByteInst)
fromInst p a i = let
    i64 = Tconst I64
    help tp = return .
        (,,,) tp p a . Right in
    case i of
        Push x    -> help i64 $ Bcode.Push $ ST.I64 x
        Builtin b -> help (builtinTyp b) $ Bcode.Builtin b
        PQuote xs -> fromInst p a (Doblk xs)
            >>= return . \ (typ, p', a', Right (Bcode.Chk by_is))
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Quote typ $ insts by_is)
        PType tlit -> typetypelit p a tlit
            >>= return . \ (p', a', typ)
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Type typ)
        Doblk  xs -> typechunk p a "do-block" Nothing xs
            >>= return . \ (("do-block",by_is):p', a')
                -> (typT by_is, p', a', Right $ Bcode.Chk by_is)

        Typblk tlit xs -> typetypelit p a tlit
            >>= \ (p', a', typ) -> fromInst p' a' (Doblk xs)
            >>= assertWith ((typ==) . (\(f, _, _, _) -> f))
                    (\ (tp, _, _, _) ->
                    "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")

        Nameblk name xs ->
            fromInst p a (Doblk xs)
            >>= return . \ (_typ, p', a', Right (Bcode.Chk chk))
                -> (Tfunc [] [], p', a', Left (name, chk))

        NameTypblk name typ xs ->
            fromInst p a (Typblk typ xs)
            >>= return . \ (_typ, p', a', Right (Bcode.Chk chk))
                -> (Tfunc [] [], p', a', Left (name, chk))

        Identifier ref -> case (find ref p, find ref a) of
            (Just chk, Nothing        ) ->
                Right (typT chk, p, a, Right (Bcode.ChkCall ref))
            (Nothing , Just (m_tl, xs)) ->
                assertWith ((==1) . length) (
                    \ a' -> "Found " ++ show (length a') ++
                    " blocks with name `" ++ ref ++ "`"
                    ) (partPair ref a)
                >>= \ (_, a')
                    -> maybe (Right (p, a', Nothing))
                    (\ tl ->
                        typetypelit p a' tl
                        >>= (\ (a1, a2, a3) -> Right (a1, a2, Just a3))) m_tl
                >>= \ (p'', a'', m_tp) -> typechunk p'' a'' ref m_tp xs
                >>= return . \ (p3, a3)
                    -> case find ref p3 of
                        Just by_is ->
                            (typT by_is, p3, a3, Right (Bcode.ChkCall ref))
                        Nothing    -> error $
                            "Typecheck.fromInst.Identifier:" ++
                            "Could't find " ++ ref
            (Nothing , Nothing        ) ->
                Left $ "Could not find `" ++ ref ++
                "` as a reference.\n\tMaybe you have a ciclic calling?\n\t"
                ++ "(They are not supported, yet)"
            (Just  _ , Just  _        ) ->
                error $ "Typecheck.fromInst.Identifier:" ++
                "Found 2 references of " ++ ref
