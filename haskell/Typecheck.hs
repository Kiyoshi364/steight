module Typecheck where

import Inst
import Utils (fork)
import Control.Monad ((>=>))

typecheck :: Program -> IO (Program, Bool)
typecheck prog = case typeblock $ code prog of
    Left  err
        -> putStrLn err >> return (Program [Halt], False)
    Right b@(Block inp out is)
        -> if inp == [] && out == []
            then putStrLn "Typecheck ok!" >> return (Program [Blk b], True)
            else putStrLn
                ("First Block should be of type [ -- ], but it's of type " ++
                tpp (inp, out))
                >> return (Program [Halt], False)

typeblock :: [Inst] -> Either String Block
typeblock = do_typeblock [] [] . Block [] []

do_typeblock :: [Inst] -> [TypeSig] -> Block -> Either String Block
do_typeblock ris st (Block inp []  []   ) =
    Right $ Block inp st $ reverse ris
do_typeblock ris st (Block inp [] (i:is)) = do
    ((inps, outs), inst) <- case i of
        Doblk xs -> typeblock xs >>= return . fork (,) instTyp id . Blk
        Typblk inp out xs ->
            typeblock xs >>= assert ((inp, out), Blk $ Block inp out xs)
                (\p -> "The typed-do-block `" ++ show (Doblk xs)
                ++ "` expected type was `"    ++ tpp' (inp, out)
                ++ "`, but actual type is `"  ++ tpp' (fst p) ++ "`")
                . fork (,) instTyp id . Blk
        _  -> return $ fork (,) instTyp id i
    case match inps st of
        Just (Left  ys) ->
            do_typeblock (inst:ris) (outs ++ []) (Block (inp ++ ys) [] is)
        Just (Right xs) ->
            do_typeblock (inst:ris) (outs ++ xs) (Block (inp      ) [] is)
        Nothing         ->
            Left $ "Instruction `" ++ show i ++ "` expected " ++ show inps ++
            " but found `" ++ show st ++ "`"
do_typeblock ris st blk = Left $ "Non-exhaustive pattern: do_typeblock (" ++
    show ris ++ ") (" ++ show st ++ ") (" ++ show blk ++ ")"

match :: Eq a => [a] -> [a] -> Maybe (Either [a] [a])
match  []       xs  = Just $ Right xs
match    ys     []  = Just $ Left  ys
match (y:ys) (x:xs)
    | x == y        = match ys xs
    | otherwise     = Nothing

assert :: Eq a => a -> (a -> String) -> a -> Either String a
assert exp f x
    | exp == x  = Right $ exp
    | otherwise = Left  $ f x
