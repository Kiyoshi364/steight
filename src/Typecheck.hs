module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), ConstT(..), compose)
import IR.AST as AST
    (AST(..), Builtin(I64b), Inst(..), AVar(..), TypeLit(..), builtinTyp)
import IR.Bytecode as Bcode
    (Bytecode(..), cons, Chunk(..), emptyChunk, ByteInst, fromBuiltin)
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
    in
    case typechunk prog ast str m_tp is of
        Left  err    -> (err:errs', prog', ast')
        Right (p, a) -> (    errs', p    , a   )

typetypelit :: ByteDict -> ASTDict -> TypeLit
    -> Either String (ByteDict, ASTDict, TypeSig)
typetypelit p a (TypeLit i o) = do
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
    f pf af (Left  (Avar  v)     ) = Right $ (,,) pf af $ Tvar   v
    f pf af (Left  (Amany v)     ) = Right $ (,,) pf af $ Tmany (v, 0)
    f pf af (Right (Builtin I64b)) = Right $ (,,) pf af $ Tconst I64
    f pf af (Right  inst         ) = case inst of
        PType   tlit -> typetypelit pf af tlit
        Identifier _ -> err "identifier"
        _            -> err "generic"
      where
        err s = Left $ "Typecheck.typetypelit: (" ++ s ++ ") instruction " ++
            show inst ++ " is not suported yet"

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
        Builtin b -> help (builtinTyp b) $ Bcode.Builtin $ fromBuiltin b
        PQuote xs -> fromInst p a (Block Nothing Nothing xs)
            >>= return . \ (typ, p', a', Right (Bcode.Chk by_is))
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Quote typ $ insts by_is)
        PType tlit -> typetypelit p a tlit
            >>= return . \ (p', a', typ)
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Type typ)
        Block m_name m_tlit xs -> do
            (p', a1) <- typechunk p a "do-block" Nothing xs
            (by_is, p1) <- case p' of
                (("do-block", by_is):p1) -> return (by_is, p1)
                (top                :_ ) -> error $
                    "Typecheck.fromInst.Block: unreacheable: " ++
                    "found `" ++ show top ++ "` at the top of the program"
                []                       -> error $
                    "Typecheck.fromInst.Block: unreacheable: " ++
                    "the program is empty"
            (p2, a2, typ) <- maybe (Right (p1, a1, typT by_is)) id $
                    typetypelit p a <$> m_tlit
            _ <- assert typ (\ tp ->
                    "The typed-" ++ maybe "do-" (const "") m_name
                    ++ "block `" ++ show (Block m_name m_tlit xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")
                $ typT by_is
            return $ maybe (typT by_is , p2, a2, Right $ Bcode.Chk by_is)
                (\ name -> (Tfunc [] [], p2, a2, Left  $ (,) name  by_is))
                m_name
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
