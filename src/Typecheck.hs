module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), ConstT(..), UserType(..), UserCase(..)
    , compose)
import IR.Identifier (Identifier(..), mk_type, mk_builtin)
import qualified IR.Identifier as Id (Type, fromNormal, fromBuiltin)
import IR.Token (Loc)
import IR.AST as AST
    (AST(..), ASTEntry(..), ASTDict
    , Builtin(I64b), Inst(..), Instruction(..), AVar(..), TypeLit(..)
    , CaseDecl(..)
    , astEntryLoc
    , builtinTyp)
import IR.Bytecode as Bcode
    (Bytecode(..), ByteEntry(..), ByteDict, cons
    , Chunk(..), emptyChunk, ByteInst, fromBuiltin)
import qualified IR.Bytecode as ST (StkTyp(..))
import qualified IR.Bytecode as Bcode (ByteInst(..))
import Utils (assert, assertWith, loop)
import Dict (insert, find, partPair)

typecheckIO :: AST -> IO (Bytecode, Bool)
typecheckIO ast = do
    (err_, prog, []) <- return (typecheck ast)
    (mainTypOk, errs) <- return $
        case find (Id.fromNormal "main") prog of
            Nothing                -> (False, err_ ++
                ["main block not found or with an error"])
            Just (ByteChunk (Chunk l typ _   _)) ->
                if typ == Tfunc [] [] then (True, err_)
                else (,) False $
                (show l ++ ": In main: main shoud have type `" ++
                show (Tfunc [] []) ++ "` but has type `" ++
                show typ ++ "`") : err_
            Just (ByteTypeDecl (UserType loc _)) ->
                (,) False $
                ("In main: main shoud be a block of type `" ++
                show (Tfunc [] []) ++ "` but it is a type" ++
                " and is defined here: " ++
                show loc) : err_
    if length errs > 0
    then putStrLn "=== Errors: ==="
        >> mapM putStrLn (map (++"\n") errs)
        >> putStrLn "===============\n"
        >> if isOk (Id.fromNormal "main") prog
            then return (Bytecode prog, mainTypOk)
            else return (Bytecode prog, False    )
    else return (Bytecode prog, mainTypOk)

isOk :: Identifier -> ByteDict -> Bool
isOk str prog = case find str prog of
    Nothing                                -> False
    Just (ByteChunk    (Chunk _ _ is scp)) -> rec is (Bcode.dict scp) prog
    Just (ByteTypeDecl _                 ) -> False
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
    in case typechunk prog' ast' str m_tp l is of
        Left  err    -> (err:errs', prog', ast')
        Right (p, a) -> (    errs', p    , a   )
iter (errs, prog, (str, (ASTTypeDecl l l_tl cs)):ast) = Right $
    case typetypedecl prog ast str l l_tl cs of
        Left  err    -> (err:errs, prog, ast)
        Right (p, a) -> (    errs, p   , a  )

typetypedecl :: ByteDict -> ASTDict -> Identifier -> Loc
    -> (Loc, TypeLit) -> [(Loc, CaseDecl)]
    -> Either String (ByteDict, ASTDict)
typetypedecl prog ast str l l_tl cs =
    checkDuplicatedName ast str l
    >> case str of
        IType name -> do_typetypedecl prog ast name l l_tl [] cs
        _          -> error $
            "Typecheck.typetypedecl: unexpected Identifier: "
            ++ show str

do_typetypedecl :: ByteDict -> ASTDict -> Id.Type -> Loc
    -> (Loc, TypeLit) -> [UserCase] -> [(Loc, CaseDecl)]
    -> Either String (ByteDict, ASTDict)
do_typetypedecl prog ast str l l_tl ucs  []    =
    let
        typ = (UserType l (reverse ucs))
        prog' :: ByteDict
        prog' = insert (IType str) (ByteTypeDecl typ) prog
    in case tl of
        TypeLit [] [(_, e_vi)] ->
            case e_vi of
                Right (Inst _ (Identifier (IType name))) ->
                    if name == (mk_type "Type") then Right ()
                    else err "should return a `Type`"
                Left _ -> err "is a type variable"
                _ -> err "weird instruction)"
        _  -> err "too many types (generic)"
    >> Right (prog', ast)
  where
    (l_t, tl) = l_tl
    err :: String -> Either String a
    err reason = Left $ show l_t
        ++ ": Typecheck.do_typetypedecl: the type of `"
        ++ show (IType str)
        ++ "` declared at " ++ show l
        ++ " namely `" ++ show tl
        ++ " is not suported yet, because: " ++ reason
do_typetypedecl prog ast str l l_tl ucs (c:cs) =
    let
        uc = UserCase c_l s
    in case t of
        TypeLit [] [(_, e_vi)] ->
            case e_vi of
                Right (Inst _ (Identifier id_name)) ->
                    if id_name == IType str then Right ()
                    else err "should return declared type"
                Left _ -> err "is a type variable"
                _ -> err "weird instruction"
        _  -> err "too many types (generic)"
    >> do_typetypedecl prog ast str l l_tl (uc:ucs) cs
  where
    (c_l, c_d) = c
    CaseDecl (s_l, s) (t_l, t) = c_d
    err :: String -> Either String a
    err reason = Left $ show t_l
        ++ ": Typecheck.do_typetypedecl: case `" ++ show s
        ++ "` named at " ++ show s_l
        ++ " inside of `" ++ show (IType str) ++ "` type"
        ++ " is not suported yet, because: " ++ reason

typetypelit :: ByteDict -> ASTDict -> (Loc, TypeLit)
    -> Either String (ByteDict, ASTDict, TypeSig)
typetypelit p a (lt, TypeLit i o) = do
    (p1, a1, tin ) <- toTypeSigList p  a  i
    (p2, a2, tout) <- toTypeSigList p1 a1 o
    return (p2, a2, Tfunc tin tout)
  where
    toTypeSigList :: ByteDict -> ASTDict -> [(Loc, Either AVar Inst)]
        -> Either String (ByteDict, ASTDict, [TypeSig])
    toTypeSigList pt at = foldl (\ pack x -> do
            (p', a', xs) <- pack
            (p'', a'', x'') <- f p' a' x
            return (p'', a'', xs ++ [x''])
        ) $ Right (pt, at, [])

    f :: ByteDict -> ASTDict -> (Loc, Either AVar Inst)
        -> Either String (ByteDict, ASTDict, TypeSig)
    f pf af (_, (Left  (Avar  v)      )) = Right $ (,,) pf af $ Tvar   v
    f pf af (_, (Left  (Amany v)      )) = Right $ (,,) pf af $ Tmany (v, 0)
    f pf af (l, (Right (Inst l2  inst))) =
        assert l (error $
            "Typecheck.typetypelit.toTypeSigList.f:"
            ++ " found non-matching locations: `"
            ++ show l ++ "` `" ++ show l2
            ++ "` with instruction " ++ show inst
         ) l2
        >> case inst of
            Builtin I64b -> Right $ (,,) pf af $ Tconst I64
            PType   tlit -> typetypelit pf af (lt, tlit)
            Identifier _ -> err "identifier"
            _            -> err "generic"
      where
        err s = Left $ show l
            ++ ": Typecheck.typetypelit: (" ++ s ++ ") instruction "
            ++ show inst ++ " is not suported in Type Literals yet"

typechunk :: ByteDict -> ASTDict -> Identifier -> Maybe TypeSig ->
    Loc -> [Inst] -> Either String (ByteDict, ASTDict)
typechunk prog ast str m_tp l is =
    checkDuplicatedName ast str l
    >> do_typechunk prog ast str m_tp emptyChunk{ bloc = l } is

do_typechunk :: ByteDict -> ASTDict -> Identifier -> Maybe TypeSig ->
    Chunk -> [Inst] -> Either String (ByteDict, ASTDict)
do_typechunk prog ast str m_tp (Chunk l stk by_is scp)  []    =
    case m_tp of
        Just tp -> assert tp (\ typ -> show l
                ++ ": The named-typed-block '" ++ show str
                ++ "' expected type was `" ++ show tp
                ++ "` but actual type is `" ++ show typ
                ++ "`") stk
            >> Right
            (insert str (ByteChunk $ Chunk l stk (reverse by_is) scp) prog, ast)
        Nothing -> Right
            (insert str (ByteChunk $ Chunk l stk (reverse by_is) scp) prog, ast)
do_typechunk prog ast str m_tp (Chunk l stk by_is scp) (i:is) =
    case fromInst prog ast i of
        Left  err -> Left $ show l ++ ": In " ++ show str ++ ": " ++ err
        Right (typ, p, a, e_sp_by_i) -> case e_sp_by_i of
            Left  s       -> do_typechunk p a str m_tp
                    (Chunk l stk      by_is  (Bcode.cons s scp)) is
            Right by_i -> case compose stk typ of
                Right st  -> do_typechunk p a str m_tp
                    (Chunk l st (by_i:by_is)               scp ) is
                Left  err ->
                    Left $ show l ++ ": In " ++ show str ++
                    ": Instruction `" ++ show i ++ "`: " ++ err

checkDuplicatedName :: ASTDict -> Identifier -> Loc -> Either String ()
checkDuplicatedName ast str l =
    assertWith (maybe True (const False) . find str)
        (\a -> show l
            ++ ": Found two blocks with name `" ++ show str ++ "`\n"
            ++ "the other one was found here: "
            ++ maybe (error "Typecheck.typechunck: unreachable")
                (show . astEntryLoc) (find str a)
            ++ "\n\tHere is the rest of the parsed AST:\n" ++ show a)
        ast
    >> return ()

fromInst :: ByteDict -> ASTDict -> Inst
    -> Either String
        (TypeSig, ByteDict, ASTDict, Either (Identifier, ByteEntry) ByteInst)
fromInst p a (Inst l i) = let
    i64 = Tconst I64
    help tp = return .
        (,,,) tp p a . Right in
    case i of
        Push x    -> help i64 $ Bcode.Push $ ST.I64 x
        Builtin b -> help (builtinTyp b) $ Bcode.Builtin $ fromBuiltin b
        PQuote xs -> fromInst p a (Inst l (Block Nothing Nothing xs))
            >>= return . \ (typ, p', a', e_sp_by_i)
                -> case e_sp_by_i of
                    Right (Bcode.Chk by_is) -> (Tfunc [] [typ], p', a',
                        Right $ Bcode.Push $ ST.Quote typ $ insts by_is)
                    _ -> error $ "Typecheck.fromInst.PQuote: unreachable: "
                            ++ show e_sp_by_i
        PType tlit -> typetypelit p a (l, tlit)
            >>= return . \ (p', a', typ)
                -> (Tfunc [] [typ], p', a',
                    Right $ Bcode.Push $ ST.Type typ)
        Block m_l_name m_l_tlit xs -> do
            (p', a1) <- typechunk p a (Id.fromBuiltin "#do-block") Nothing l xs
            (by_is, p1) <- case p' of
                (top@(IBuiltin builtin, ByteChunk by_is):p1)
                         -> if builtin == mk_builtin "#do-block"
                    then return (by_is, p1)
                    else error $
                        "Typecheck.fromInst.Block: unreacheable: " ++
                        "expected \"do-block\" at the top of the program" ++
                        "found `" ++ show top ++ "`"
                (top:_ ) -> error $
                    "Typecheck.fromInst.Block: unreacheable: " ++
                    "found `" ++ show top ++ "` at the top of the program"
                []       -> error $
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
                (\ name -> (Tfunc [] [], p2, a2, Left  $ (,) name $ ByteChunk by_is))
                (fmap (INormal . snd) m_l_name)
        TypeDecl name typ cs -> error $
            "NOT IMPLEMENTED: Typecheck.fromInst.TypeDecl: "
            ++ "name: " ++ show name ++ "; typ: " ++ show typ
            ++ "; cases: " ++ show cs
        Identifier ref -> case (find ref p, find ref a) of
            (Just (ByteChunk chk)    , Nothing                       ) ->
                Right (typT chk, p, a, Right (Bcode.ChkCall ref))
            (Just (ByteTypeDecl typ) , Nothing                       ) ->
                error (
                    "NOT IMPLEMENTED: Typecheck.fromInst.Identifier.ByteTypeDecl"
                ) typ
            (Nothing                 , Just (ASTBlock l_i m_l_tl xs) ) ->
                assertWith ((==1) . length) (
                    \ a' -> error $
                    "Typecheck.fromInst.Identifier.ASTBlock: unreacheable: "
                    ++ "Found " ++ show (length a')
                    ++ " blocks with name `" ++ show ref ++ "`"
                    ) (partPair ref a)
                >>= \ (_, a')
                    -> maybe (Right (p, a', Nothing))
                    (\ tl ->
                        typetypelit p a' tl
                        >>= (\ (a1, a2, a3) -> Right (a1, a2, Just a3))) m_l_tl
                >>= \ (p'', a'', m_tp) -> typechunk p'' a'' ref m_tp l_i xs
                >>= return . \ (p3, a3)
                    -> case find ref p3 of
                        Just (ByteChunk by_is) ->
                            (typT by_is, p3, a3, Right (Bcode.ChkCall ref))
                        Just (ByteTypeDecl typ) ->
                            error (
                                "NOT IMPLEMENTED: Typecheck.fromInst.Identifier.ByteTypeDecl"
                            ) typ
                        Nothing    -> error $
                            "Typecheck.fromInst.Identifier.ASTBlock: "
                            ++ "Could't find " ++ show ref
            (Nothing                 , Just (ASTTypeDecl l_i l_tl xs)) ->
                error (
                    "NOT IMPLEMENTED: Typecheck.fromInst.Identifier.ASTTypeDecl"
                ) l_i l_tl xs
            (Nothing                 , Nothing                       ) ->
                Left $ "Could not find `" ++ show ref ++
                "` as a reference.\n\tMaybe you have a ciclic calling?\n\t"
                ++ "(They are not supported, yet)"
            (Just  _                 , Just  _                       ) ->
                error $ "Typecheck.fromInst.Identifier.Both_Just:"
                ++ " Found 2 references of " ++ show ref
