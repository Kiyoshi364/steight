module Typecheck where

import Inst
import IR (Program(..), Block(..), IRInst, blockTyp)
import qualified IR
import Utils (fork, assert, assertWith, mapFst, mapSnd)
import Control.Monad ((>=>))

typecheck :: AST -> IO (Program, Bool)
typecheck ast = return ast
    >>= return . map (mapSnd typeblock) . Inst.dict
    >>= foldr (\ (name, blk) io_pair -> putStr (name ++ ": ") >>
        case blk of
        Left  err -> putStrLn (err) >> fmap (mapSnd $ const False) io_pair
        Right b@(Block i o is) -> if (i == [] && o == []) || name /= "main"
            then putStrLn "Ok!" >> fmap (mapFst ((name, b):)) io_pair
            else putStrLn
                ("Main Block should be of type [ -- ], but it's of type "
                ++ tpp (i, o)) >> fmap (mapSnd $ const False) io_pair
        ) (return ([], True))
    >>= either (fmap (const ([], False)) . putStrLn) return
        . assertWith (const $ or . map ((=="main") . fst) . fst)
            () (const "Main Block not found")
    >>= return . mapFst Program

typeblock :: [Inst] -> Either String Block
typeblock = do_typeblock [] [] []

do_typeblock ::
    [IRInst] -> [TypeSig] -> [TypeSig] -> [Inst] -> Either String Block
do_typeblock ir_is inp st  []    = Right $ Block inp st $ reverse ir_is
do_typeblock ir_is inp st (i:is) = do
    ((inps, outs), ir_i) <- fromInst i
    case match inps st of
        Just (Left  ys) ->
            do_typeblock (ir_i:ir_is) (inp ++ ys) (outs ++ []) is
        Just (Right xs) ->
            do_typeblock (ir_i:ir_is) (inp      ) (outs ++ xs) is
        Nothing         ->
            Left $ "Instruction `" ++ show i ++ "` expected " ++ show inps ++
            " but found `" ++ show st ++ "`"

match :: Eq a => [a] -> [a] -> Maybe (Either [a] [a])
match  []       xs  = Just $ Right xs
match    ys     []  = Just $ Left  ys
match (y:ys) (x:xs)
    | x == y        = match ys xs
    | otherwise     = Nothing

fromInst :: Inst -> Either String (([TypeSig], [TypeSig]), IRInst)
fromInst i = let help = return . (,) (instTyp i) in case i of
    Push x    -> help $ IR.Push x
    Swap      -> help $ IR.Swap
    Dup       -> help $ IR.Dup
    Drop      -> help $ IR.Drop
    Print     -> help $ IR.Print
    Halt      -> help $ IR.Halt
    Builtin b -> help $ IR.Builtin b
    Doblk xs -> typeblock xs >>= return . fork (,) blockTyp IR.Blk
    Typblk inp out xs ->
            typeblock xs >>= fork (>>) (
                assert (inp, out)
                    (\p -> "The typed-do-block `" ++ show (Doblk xs)
                    ++ "` expected type was `"    ++ tpp' (inp, out)
                    ++ "`, but actual type is `"  ++ tpp' p ++ "`")
                . blockTyp
                ) (return . fork (,) blockTyp IR.Blk)
