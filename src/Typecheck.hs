module Typecheck where

import Types (TypeSig(..), compose)
import Inst (AST(..), Inst(..), instTyp)
import IR (Program(..), Block(..), IRInst)
import qualified IR
import Utils (fork, assert, onFst, loop)
import Dict (Dict, insert, find)

type IDict  = Dict String [Inst]
type IRDict = Dict String Block

typecheck :: AST -> IO (Program, Bool)
typecheck ast =
        (return $ fst $ loop iter ([], [], Inst.dict ast))
    >>= fork const (\ (a, b, _) -> return (b, a == []))
            (mapM putStrLn . \ (a, _, _) -> a)
    >>= (return . onFst Program)

iter :: ([String], IRDict, IDict) -> Either () ([String], IRDict, IDict)
iter (   _,    _,  []          ) = Left ()
iter (errs, prog, (str, is):ast) = Right $
    case typeblock prog ast str is of
        Left  err    -> (err:errs, prog, ast)
        Right (p, a) -> (    errs, p   , a  )

typeblock :: IRDict -> IDict -> String -> [Inst]
    -> Either String (IRDict, IDict)
typeblock prog ast str = do_typeblock prog ast str [] $ Tfunc [] []

do_typeblock :: IRDict -> IDict -> String ->
    [IRInst] -> TypeSig -> [Inst] -> Either String (IRDict, IDict)
do_typeblock prog ast str ir_is stack  []    =
    Right $ (insert str (Block stack $ reverse ir_is) prog, ast)
do_typeblock prog ast str ir_is stack (i:is) = do
    (typ, p, a, ir_i) <- fromInst prog ast i
    case compose stack typ of
        Right st  -> do_typeblock p a str (ir_i:ir_is) st is
        Left  err ->
            Left $ "In " ++ str ++
            ": Instruction `" ++ show i ++ "`: " ++ err

fromInst :: IRDict -> IDict -> Inst
    -> Either String (TypeSig, IRDict, IDict, IRInst)
fromInst p a i = let help = return . (,,,) (instTyp i) p a in
    case i of
        Push x    -> help $ IR.Push x
        Swap      -> help $ IR.Swap
        Dup       -> help $ IR.Dup
        Drop      -> help $ IR.Drop
        Print     -> help $ IR.Print
        Halt      -> help $ IR.Halt
        Builtin b -> help $ IR.Builtin b
        Doblk xs -> typeblock p a "" xs
            >>= return . \ (("",is):irds, ids)
                -> (typT is, irds, ids, IR.Blk is)
        Typblk typ xs ->
            typeblock p a "" xs
            >>= fork (>>) (
                assert typ
                    (\tp -> "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show tp ++ "`")
                . maybe (error "Typecheck.fromInst: case Typeblk") typT
                . find (=="") . fst
                ) (return . \ (("",is):irds, ids)
                    -> (typT is, irds, ids, IR.Blk is))
