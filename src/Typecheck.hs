module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), compose)
import Inst (AST(..), Inst(..), instTyp)
import IR.Bytecode as Bcode
    (Bytecode(..), cons, Chunk(..), emptyChunk, ByteInst)
import qualified IR.Bytecode as ST (StkTyp(..))
import qualified IR.Bytecode as IR (ByteInst(..))
import Utils (assert, assertWith, loop)
import Dict (Dict, insert, find, partPair)

type IDict  = Dict String (Maybe TypeSig, [Inst])
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
        IR.ChkCall s -> rec is scp p && (isOk s p || isOk s scp)
        _            -> rec is scp p

typecheck :: AST -> ([String], ByteDict, IDict)
typecheck ast = fst $ loop iter ([], [], Inst.dict ast)

iter :: ([String], ByteDict, IDict) -> Either () ([String], ByteDict, IDict)
iter (   _,    _,  []          ) = Left ()
iter (errs, prog, (str, (m_tp, is)):ast) = Right $
    case typeblock prog ast str m_tp is of
        Left  err    -> (err:errs, prog, ast)
        Right (p, a) -> (    errs, p   , a  )

typeblock :: ByteDict -> IDict -> String -> Maybe TypeSig ->
    [Inst] -> Either String (ByteDict, IDict)
typeblock prog ast str m_tp is =
    assertWith (maybe True (const False) . find str)
        (\a -> "Found two blocks with name `" ++ str ++ "`\n\t" ++ show a)
        ast
    >> do_typeblock prog ast str m_tp emptyChunk is

do_typeblock :: ByteDict -> IDict -> String -> Maybe TypeSig ->
    Chunk -> [Inst] -> Either String (ByteDict, IDict)
do_typeblock prog ast str m_tp (Chunk stk ir_is scp)  []    =
    case m_tp of
        Just tp -> assert tp (\ typ ->
                "The named-typed-block '" ++ str ++
                "' expected type was `" ++
                show tp ++ "` but actual type is `" ++
                show typ ++ "`") stk
            >> Right
            (insert str (Chunk stk (reverse ir_is) scp) prog, ast)
        Nothing -> Right
            (insert str (Chunk stk (reverse ir_is) scp) prog, ast)
do_typeblock prog ast str m_tp (Chunk stk ir_is scp) (i:is) =
    case fromInst prog ast i of
        Left  err -> Left $ "In " ++ str ++ ": " ++ err
        Right (typ, p, a, e_sp_ir_i) -> case e_sp_ir_i of
            Left  s       -> do_typeblock p a str m_tp
                    (Chunk stk      ir_is  (Bcode.cons s scp)) is
            Right ir_i -> case compose stk typ of
                Right st  -> do_typeblock p a str m_tp
                    (Chunk st (ir_i:ir_is)               scp ) is
                Left  err ->
                    Left $ "In " ++ str ++
                    ": Instruction `" ++ show i ++ "`: " ++ err

fromInst :: ByteDict -> IDict -> Inst
    -> Either String (TypeSig, ByteDict, IDict, Either (String, Chunk) ByteInst)
fromInst p a i = let
    help = return .
        (,,,) (instTyp i) p a . Right in
    case i of
        Push x    -> help $ IR.Push $ ST.I64 x
        Builtin b -> help $ IR.Builtin b
        PQuote xs -> fromInst p a (Doblk xs)
            >>= return . \ (typ, p', a', Right (IR.Chk ir_is))
                -> (Tfunc [] [typ], p', a',
                    Right (IR.Push $ ST.Quote typ $ insts ir_is))
        Doblk  xs -> typeblock p a "do-block" Nothing xs
            >>= return . \ (("do-block",ir_is):p', a')
                -> (typT ir_is, p', a', Right $ IR.Chk ir_is)

        Typblk typ xs ->
            fromInst p a (Doblk xs)
            >>= assertWith ((typ==) . (\(f, _, _, _) -> f))
                    (\ (tp, _, _, _) ->
                    "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")

        Nameblk name xs ->
            fromInst p a (Doblk xs)
            >>= return . \ (_typ, p', a', Right (IR.Chk chk))
                -> (Tfunc [] [], p', a', Left (name, chk))

        NameTypblk name typ xs ->
            fromInst p a (Typblk typ xs)
            >>= return . \ (_typ, p', a', Right (IR.Chk chk))
                -> (Tfunc [] [], p', a', Left (name, chk))

        Identifier ref -> case (find ref p, find ref a) of
            (Just chk, Nothing        ) ->
                Right (typT chk, p, a, Right (IR.ChkCall ref))
            (Nothing , Just (m_tp, xs)) ->
                assertWith ((==1) . length) (
                    \ a' -> "Found " ++ show (length a') ++
                    " blocks with name `" ++ ref ++ "`"
                    ) (partPair ref a)
                >>= (\ (_, a') -> typeblock p a' ref m_tp xs)
                >>= return . \ (p', a')
                    -> case find ref p' of
                        Just ir_is ->
                            (typT ir_is, p', a', Right (IR.ChkCall ref))
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
