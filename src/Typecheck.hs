module Typecheck where

import Types (TypeSig(..), compose)
import Inst (AST(..), Inst(..), instTyp)
import IR (Scope(..), Block(..), emptyBlock, IRInst)
import qualified IR
import Utils (fork, assertWith, onFst, loop)
import Dict (Dict, insert)

type IDict  = Dict String [Inst]
type IRDict = Dict String Block

typecheck :: AST -> IO (Scope, Bool)
typecheck ast =
        (return $ fst $ loop iter ([], [], Inst.dict ast))
    >>= fork const (\ (a, b, _) -> return (b, a == []))
            (mapM putStrLn . \ (a, _, _) -> a)
    >>= (return . onFst Scope)

iter :: ([String], IRDict, IDict) -> Either () ([String], IRDict, IDict)
iter (   _,    _,  []          ) = Left ()
iter (errs, prog, (str, is):ast) = Right $
    case typeblock prog ast str is of
        Left  err    -> (err:errs, prog, ast)
        Right (p, a) -> (    errs, p   , a  )

typeblock :: IRDict -> IDict -> String -> [Inst]
    -> Either String (IRDict, IDict)
typeblock prog ast str = do_typeblock prog ast str emptyBlock

do_typeblock :: IRDict -> IDict -> String ->
    Block -> [Inst] -> Either String (IRDict, IDict)
do_typeblock prog ast str (Block stk ir_is scp)  []    =
    Right $ (insert str (Block stk (reverse ir_is) scp) prog, ast)
do_typeblock prog ast str (Block stk ir_is scp) (i:is) = do
        (typ, p, a, e_sp_ir_i) <- fromInst prog ast i
        case e_sp_ir_i of
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
            >>= return . \ (("",is):p', a')
                -> (typT is, p', a', Right $ IR.Blk is)
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
