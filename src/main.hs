module Main (main) where

import qualified Parser as P
import Inst (Inst, Inst(..), Builtin(..), lexer, AST(..))
import IR (Program(Program), Block(..))
import Typecheck (typecheck, typeblock)
import Simulation (simulate)
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
        Left  l -> putStrLn l >> return (AST [("main", [])])
        Right r ->               return  r
    putStrLn $ show prog
    (p', ok) <- typecheck prog
    putStrLn $ show p'
    if ok then putStrLn "=== simulation ===" >> simulate p'
          else simulate $ Program [("main", Block [] [] [])]
    return ()

iprog :: [Inst]
iprog = [
    Doblk [Push 2, Push 1, Builtin Add],
    Dup, Print, Push 3, Swap, Builtin Sub, Print
    ]

bprog :: Block
bprog = either (const $ Block [] [] []) id $ typeblock iprog

tprog :: Program
tprog = Program [("main", bprog)]

pp :: AST -> String
pp = foldr (\ t -> ((show t ++ ", ") ++)) "" . dict
