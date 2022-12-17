module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), ConstT(..), compose)
import IR.Token (Loc)
import IR.AST as AST
    (AST(..), ASTEntry(..), ASTDict
    , Builtin(I64b), Inst(..), Instruction(..), AVar(..), TypeLit(..)
    , astEntryLoc
    , builtinTyp)
import IR.Bytecode as Bcode
    (Bytecode(..), ByteDict, cons
    , Chunk(..), emptyChunk, ByteInst, fromBuiltin)
import qualified IR.Bytecode as ST (StkTyp(..))
import qualified IR.Bytecode as Bcode (ByteInst(..))
import Utils (assert, assertWith, loop)
import Dict (insert, find, partPair)

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
iter (errs, prog, (str, (ASTBlock l m_l_tl is)):ast) = Right $
    let (errs', prog', ast', m_tp) = maybe (errs, prog, ast, Nothing) (
            \ (tl, tlit) -> case typetypelit prog ast (tl, tlit) of
                Left er          -> (er:errs, prog, ast, Nothing)
                Right (p, a, tp) -> (   errs, p   , a  , Just tp)
            )
            m_l_tl
    in case typechunk prog ast str m_tp l is of
        Left  err    -> (err:errs', prog', ast')
        Right (p, a) -> (    errs', p    , a   )
iter (errs, prog, (str, (ASTTypeDecl l l_tl cs)):ast) = Right $
    error (
        "NOT IMPLEMENTED: Typecheck.iter.ASTTypeDecl"
    ) errs prog str l l_tl cs ast

typetypelit :: ByteDict -> ASTDict -> (Loc, TypeLit)
    -> Either String (ByteDict, ASTDict, TypeSig)
typetypelit p a (lt, TypeLit i o) = do
    (p1, a1, tin ) <- toTypeSigList p  a  i
    (p2, a2, tout) <- toTypeSigList p1 a1 o
    return (p2, a2, Tfunc tin tout)
  where
    toTypeSigList :: ByteDict -> ASTDict -> [Either AVar Inst]
        -> Either String (ByteDict, ASTDict, [TypeSig])
    toTypeSigList pt at = foldl (\ pack x -> do
            (p', a', xs) <- pack
            (p'', a'', x'') <- f p' a' x
            return (p'', a'', xs ++ [x''])
        ) $ Right (pt, at, [])
    f :: ByteDict -> ASTDict -> Either AVar Inst
        -> Either String (ByteDict, ASTDict, TypeSig)
    f pf af (Left  (Avar  v)              ) = Right $ (,,) pf af $ Tvar   v
    f pf af (Left  (Amany v)              ) = Right $ (,,) pf af $ Tmany (v, 0)
    f pf af (Right (Inst _ (Builtin I64b))) = Right $ (,,) pf af $ Tconst I64
    f pf af (Right (Inst l  inst         )) = case inst of
        PType   tlit -> typetypelit pf af (lt, tlit)
        Identifier _ -> err "identifier"
        _            -> err "generic"
      where
        err s = Left $ show l
            ++ ": Typecheck.typetypelit: (" ++ s ++ ") instruction "
            ++ show inst ++ " is not suported in Type Literals yet"

typechunk :: ByteDict -> ASTDict -> String -> Maybe TypeSig ->
    Loc -> [Inst] -> Either String (ByteDict, ASTDict)
typechunk prog ast str m_tp l is =
    assertWith (maybe True (const False) . find str)
        (\a -> show l
            ++ ": Found two blocks with name `" ++ str ++ "`\n"
            ++ "the other one was found here: "
            ++ maybe (error "Typecheck.typechunck: unreachable")
                (show . astEntryLoc) (find str a)
            ++ "\n\tHere is the rest of the parsed AST:\n" ++ show a)
        ast
    >> do_typechunk prog ast str m_tp emptyChunk l is

do_typechunk :: ByteDict -> ASTDict -> String -> Maybe TypeSig ->
    Chunk -> Loc -> [Inst] -> Either String (ByteDict, ASTDict)
do_typechunk prog ast str m_tp (Chunk stk by_is scp) l  []    =
    case m_tp of
        Just tp -> assert tp (\ typ -> show l
                ++ ": The named-typed-block '" ++ str
                ++ "' expected type was `" ++ show tp
                ++ "` but actual type is `" ++ show typ
                ++ "`") stk
            >> Right
            (insert str (Chunk stk (reverse by_is) scp) prog, ast)
        Nothing -> Right
            (insert str (Chunk stk (reverse by_is) scp) prog, ast)
do_typechunk prog ast str m_tp (Chunk stk by_is scp) l (i:is) =
    case fromInst prog ast i of
        Left  err -> Left $ show l ++ ": In " ++ str ++ ": " ++ err
        Right (typ, p, a, e_sp_by_i) -> case e_sp_by_i of
            Left  s       -> do_typechunk p a str m_tp
                    (Chunk stk      by_is  (Bcode.cons s scp)) l is
            Right by_i -> case compose stk typ of
                Right st  -> do_typechunk p a str m_tp
                    (Chunk st (by_i:by_is)               scp ) l is
                Left  err ->
                    Left $ show l ++ ": In " ++ str ++
                    ": Instruction `" ++ show i ++ "`: " ++ err

fromInst :: ByteDict -> ASTDict -> Inst
    -> Either String
        (TypeSig, ByteDict, ASTDict, Either (String, Chunk) ByteInst)
fromInst p a (Inst l i) = let
    i64 = Tconst I64
    help tp = return .
        (,,,) tp p a . Right in
    case i of
        Push x    -> help i64 $ Bcode.Push $ ST.I64 x
        Builtin b -> help (builtinTyp b) $ Bcode.Builtin $ fromBuiltin b
        PQuote xs -> fromInst p a (Inst l (Block Nothing Nothing xs))
            >>= return . \ (typ, p', a', Right (Bcode.Chk by_is))
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Quote typ $ insts by_is)
        PType tlit -> typetypelit p a (l, tlit)
            >>= return . \ (p', a', typ)
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Type typ)
        Block m_l_name m_l_tlit xs -> do
            (p', a1) <- typechunk p a "do-block" Nothing l xs
            (by_is, p1) <- case p' of
                (("do-block", by_is):p1) -> return (by_is, p1)
                (top                :_ ) -> error $
                    "Typecheck.fromInst.Block: unreacheable: " ++
                    "found `" ++ show top ++ "` at the top of the program"
                []                       -> error $
                    "Typecheck.fromInst.Block: unreacheable: " ++
                    "the program is empty"
            (p2, a2, typ) <- maybe (Right (p1, a1, typT by_is)) id $
                    typetypelit p a <$> m_l_tlit
            _ <- assert typ (\ tp ->
                    "The typed-" ++ maybe "do-" (const "") m_l_name
                    ++ "block `" ++ show (Block m_l_name m_l_tlit xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")
                $ typT by_is
            return $ maybe (typT by_is , p2, a2, Right $ Bcode.Chk by_is)
                (\ name -> (Tfunc [] [], p2, a2, Left  $ (,) name  by_is))
                (fmap snd m_l_name)
        -- Doblk  xs -> typechunk p a "do-block" Nothing xs
        --     >>= return . \ (("do-block",by_is):p', a')
        --         -> (typT by_is, p', a', Right $ Bcode.Chk by_is)
        -- Typblk tlit xs -> typetypelit p a tlit
        --     >>= \ (p', a', typ) -> fromInst p' a' (Doblk xs)
        --     >>= assertWith ((typ==) . (\(f, _, _, _) -> f))
        --             (\ (tp, _, _, _) ->
        --             "The typed-do-block `" ++ show (Doblk xs)
        --             ++ "` expected type was `"    ++ show typ
        --             ++ "`, but actual type is `"  ++ show tp ++ "`")
        -- Nameblk name xs ->
        --     fromInst p a (Doblk xs)
        --     >>= return . \ (_typ, p', a', Right (Bcode.Chk chk))
        --         -> (Tfunc [] [], p', a', Left (name, chk))
        -- NameTypblk name typ xs ->
        --     fromInst p a (Typblk typ xs)
        --     >>= return . \ (_typ, p', a', Right (Bcode.Chk chk))
        --         -> (Tfunc [] [], p', a', Left (name, chk))

        TypeDecl name typ cs -> error $
            "Typecheck.fromInst.TypeDecl: not implemented: "
            ++ "name: " ++ show name ++ "; typ: " ++ show typ
            ++ "; cases: " ++ show cs
        Identifier ref -> case (find ref p, find ref a) of
            (Just chk, Nothing        ) ->
                Right (typT chk, p, a, Right (Bcode.ChkCall ref))
            (Nothing , Just (ASTBlock l_i m_l_tl xs)) ->
                assertWith ((==1) . length) (
                    \ a' -> error $
                    "Typecheck.fromInst.IdentifierASTBlock: unreacheable: "
                    ++ "Found " ++ show (length a')
                    ++ " blocks with name `" ++ ref ++ "`"
                    ) (partPair ref a)
                >>= \ (_, a')
                    -> maybe (Right (p, a', Nothing))
                    (\ tl ->
                        typetypelit p a' tl
                        >>= (\ (a1, a2, a3) -> Right (a1, a2, Just a3))) m_l_tl
                >>= \ (p'', a'', m_tp) -> typechunk p'' a'' ref m_tp l_i xs
                >>= return . \ (p3, a3)
                    -> case find ref p3 of
                        Just by_is ->
                            (typT by_is, p3, a3, Right (Bcode.ChkCall ref))
                        Nothing    -> error $
                            "Typecheck.fromInst.Identifier.ASTBlock: "
                            ++ "Could't find " ++ ref
            (Nothing , Just (ASTTypeDecl l_i l_tl xs)) ->
                error (
                    "NOT IMPLEMENTED: Typecheck.fromInst.Identifier.ASTTypeDecl"
                ) l_i l_tl xs
            (Nothing , Nothing        ) ->
                Left $ "Could not find `" ++ ref ++
                "` as a reference.\n\tMaybe you have a ciclic calling?\n\t"
                ++ "(They are not supported, yet)"
            (Just  _ , Just  _        ) ->
                error $ "Typecheck.fromInst.Identifier.Both_Just:"
                ++ " Found 2 references of " ++ ref
