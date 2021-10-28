module Main (main) where

import qualified Parser as P
import Inst (Inst, Inst(..), Builtin(..), Block(..), lexer, Program(..))
import Simulation (simulate, begin)
import Typecheck (typecheck)
import Control.Applicative
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    path <- if args == [] then putStrLn "empty args" >> return ""
                           else return $ head args
    line <- readFile path
    -- putStrLn "Args:"
    -- mapM putStrLn args
    -- putStr "reading: "
    -- line <- getLine
    input <- return $ P.mkPathInput path line
    -- putStrLn $ (++) "input: " $ show input
    parsed <- return $ P.runP lexer input
    -- putStrLn $ (++) "parsed: " $ either ("error: "++) (pp) $ P.value parsed
    -- putStrLn $ (++) "leftover: " $ show $ P.input parsed
    prog <- case P.value parsed of
        Left  l -> putStrLn l >> (return $ Program [Halt])
        Right r ->                return $ Program r
    putStrLn $ show prog
    (p', ok) <- typecheck prog
    putStrLn $ show p'
    if ok then putStrLn "=== simulation ===" >> simulate p'
          else return $ begin $ Program [Halt]
    return ()

tprog :: Program
tprog = Program iprog

iprog :: [Inst]
iprog = [
    Doblk [Push 2, Push 1, Builtin Add],
    Dup, Print, Push 3, Swap, Builtin Sub, Print
    ]

pp :: Program -> String
pp = foldr (\ t -> ((show t ++ ", ") ++)) "" . code
