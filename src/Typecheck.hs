module Typecheck
    ( typecheckIO, typecheck
    )where

import Types (TypeSig(..), compose)
import Inst (AST(..), Inst(..), instTyp)
import IR (Scope(..), Block(..), emptyBlock, IRInst)
import qualified IR
import Utils (assertWith, loop)
import Dict (Dict, insert, find, partPair)

type IDict  = Dict String [Inst]
type IRDict = Dict String Block

typecheckIO :: AST -> IO (Scope, Bool)
typecheckIO ast = do
    (errs, prog, leftover) <- return (typecheck ast)
    _ <- if length errs == length leftover then return ()
        else error $ "Typecheck.typecheckIO: lenght errs (" ++
            show (length errs) ++ ") /= length lefover (" ++
            show (length leftover) ++ ")"
    _ <- if length errs > 0
        then putStrLn "\n=== Errors: ==="
            >> mapM putStrLn errs
            >> putStrLn ""
        else return ()
    return (Scope prog, leftover == [])

typecheck :: AST -> ([String], IRDict, IDict)
typecheck ast = fst $ loop iter ([], [], Inst.dict ast)

iter :: ([String], IRDict, IDict) -> Either () ([String], IRDict, IDict)
iter (   _,    _,  []          ) = Left ()
iter (errs, prog, (str, is):ast) = Right $
    case typeblock prog ast str is of
        Left  err    -> (err:errs, prog, ast)
        Right (p, a) -> (    errs, p   , a  )

typeblock :: IRDict -> IDict -> String -> [Inst]
    -> Either String (IRDict, IDict)
typeblock prog ast str is =
    assertWith (maybe True (const False) . find str)
        (\a -> "Found two blocks with name `" ++ str ++ "`\n\t" ++ show a)
        ast
    >> do_typeblock prog ast str emptyBlock is

do_typeblock :: IRDict -> IDict -> String ->
    Block -> [Inst] -> Either String (IRDict, IDict)
do_typeblock prog ast str (Block stk ir_is scp)  []    =
    Right $ (insert str (Block stk (reverse ir_is) scp) prog, ast)
do_typeblock prog ast str (Block stk ir_is scp) (i:is) =
    case fromInst prog ast i of
        Left  err -> Left $ "In " ++ str ++ ": " ++ err
        Right (typ, p, a, e_sp_ir_i) -> case e_sp_ir_i of
            Left  s       -> do_typeblock p a str
                    (Block stk      ir_is  (s:scp)) is
            Right ir_i -> case compose stk typ of
                Right st  -> do_typeblock p a str
                    (Block st (ir_i:ir_is)    scp ) is
                Left  err ->
                    Left $ "In " ++ str ++
                    ": Instruction `" ++ show i ++ "`: " ++ err

fromInst :: IRDict -> IDict -> Inst
    -> Either String (TypeSig, IRDict, IDict, Either (String, Block) IRInst)
fromInst p a i = let
    help = return .
        (,,,) (instTyp i) p a . Right in
    case i of
        Push x    -> help $ IR.Push x
        Swap      -> help $ IR.Swap
        Dup       -> help $ IR.Dup
        Drop      -> help $ IR.Drop
        Print     -> help $ IR.Print
        Halt      -> help $ IR.Halt
        Builtin b -> help $ IR.Builtin b
        Doblk xs -> typeblock p a "" xs
            >>= return . \ (("",ir_is):p', a')
                -> (typT ir_is, p', a', Right $ IR.Blk ir_is)

        Typblk typ xs ->
            fromInst p a (Doblk xs)
            >>= assertWith ((typ==) . (\(f, _, _, _) -> f))
                    (\ (tp, _, _, _) ->
                    "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")

        Nameblk name xs ->
            fromInst p a (Doblk xs)
            >>= return . \ (_typ, p', a', Right (IR.Blk blk))
                -> (Tfunc [] [], p', a', Left (name, blk))

        Identifier ref -> case (find ref p, find ref a) of
            (Just blk, Nothing ) ->
                Right (typT blk, p, a, Right (IR.BlkCall ref))
            (Nothing , Just xs ) ->
                assertWith ((==1) . length) (
                    \ a' -> "Found " ++ show (length a') ++
                    " blocks with name `" ++ ref ++ "`"
                    ) (partPair ref a)
                >>= (\ (_, a') -> typeblock p (a') ref xs)
                >>= return . \ (p', a')
                    -> case find ref p' of
                        Just ir_is ->
                            (typT ir_is, p', a', Right (IR.BlkCall ref))
                        Nothing    -> error $
                            "Typecheck.fromInst.Identifier:" ++
                            "Could't find " ++ ref
            (Nothing , Nothing ) ->
                Left $ "Could not find `" ++ ref ++
                "` as a reference.\n\tMaybe you have a ciclic calling?\n\t"
                ++ "(They are not supported, yet)"
            (Just  _ , Just  _ ) ->
                error $ "Typecheck.fromInst.Identifier:" ++
                "Found 2 references of " ++ ref
