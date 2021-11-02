module Typecheck where

import Types (TypeSig(..), compose, toPair, fromPair)
import Inst (AST(..), Inst(..), instTyp)
import IR (Program(..), Block(..), IRInst)
import qualified IR
import Utils (fork, assert, assertWith, mapFst, mapSnd)
import Control.Monad ((>=>))

typecheck :: AST -> IO (Program, Bool)
typecheck ast = return ast
    >>= return . map (mapSnd typeblock) . Inst.dict
    >>= foldr (\ (name, blk) io_pair -> putStr (name ++ ": ") >>
        case blk of
        Left  err -> putStrLn (err) >> fmap (mapSnd $ const False) io_pair
        Right b@(Block typ is) ->
            if (toPair typ == ([], [])) || name /= "main"
            then putStrLn "Ok!" >> fmap (mapFst ((name, b):)) io_pair
            else putStrLn
                ("Main Block should be of type [ -- ], but it's of type "
                ++ show typ) >> fmap (mapSnd $ const False) io_pair
        ) (return ([], True))
    >>= either (fmap (const ([], False)) . putStrLn) return
        . assertWith (const $ or . map ((=="main") . fst) . fst)
            () (const "Main Block not found")
    >>= return . mapFst Program

typeblock :: [Inst] -> Either String Block
typeblock = do_typeblock [] $ Tfunc [] []

do_typeblock ::
    [IRInst] -> TypeSig -> [Inst] -> Either String Block
do_typeblock ir_is stack  []    =
    Right $ Block stack $ reverse ir_is
do_typeblock ir_is stack (i:is) = do
    (typ, ir_i) <- fromInst i
    case compose stack typ of
        Right st  -> do_typeblock (ir_i:ir_is) st is
        Left  err ->
            Left $ "Instruction `" ++ show i ++ "`: " ++ err

fromInst :: Inst -> Either String (TypeSig, IRInst)
fromInst i = let help = return . (,) (instTyp i) in
    case i of
        Push x    -> help $ IR.Push x
        Swap      -> help $ IR.Swap
        Dup       -> help $ IR.Dup
        Drop      -> help $ IR.Drop
        Print     -> help $ IR.Print
        Halt      -> help $ IR.Halt
        Builtin b -> help $ IR.Builtin b
        Doblk xs -> typeblock xs >>= return . fork (,) typT IR.Blk
        Typblk typ xs ->
            typeblock xs >>= fork (>>) (
                assert typ
                    (\p -> "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ show typ
                    ++ "`, but actual type is `"  ++ show p ++ "`")
                . typT
                ) (return . fork (,) typT IR.Blk)
