module Main where

import qualified Parser as P
import Inst (Inst, Inst(Halt), instP, Program)
import Simulation (simulate)
import Control.Applicative
import System.Environment

main :: IO ()
main = do
    -- args <- getArgs
    -- putStrLn "Args:"
    -- mapM putStrLn args
    putStr "reading: "
    line <- getLine
    input <- return $ P.mkInput line
    putStrLn $ (++) "input: " $ P.pp input
    parsed <- return $ P.runP lexer input
    putStrLn $ (++) "parsed: " $ either ("error: "++) pp $ P.value parsed
    prog <- return $ case P.value parsed of
        Left  _ -> []
        Right x -> x
    putStrLn "\n === simulation ==="
    simulate prog
    return ()

lexer :: P.Parser Program
lexer = fmap (zip [0..] . (++[Halt])) $ some $ (P.optP P.wsP) *> instP

pp :: Program -> String
pp = foldr (\ t -> ((show (snd t) ++ ", ") ++)) ""
