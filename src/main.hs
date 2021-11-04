module Main
    ( main
    , iprog, bprog, tprog
    ) where

import qualified Parser as P
import Inst (Inst, Inst(..), Builtin(..), lexer, AST(..))
import IR (Scope(Scope), Block(..), emptyBlock)
import Typecheck (typecheck, typeblock)
import Simulation (simulate)
import Dict (Dict)
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
    _ <- if ok then putStrLn "=== simulation ===" >> simulate p'
          else simulate $ Scope [("main", emptyBlock)]
    return ()

iprog :: [Inst]
iprog = [
    Doblk [Push 2, Push 1, Builtin Add],
    Dup, Print, Push 3, Swap, Builtin Sub, Print
    ]

bprog :: Dict String Block
bprog = either (const [("", emptyBlock)]) fst $ typeblock [] [] "main" iprog

tprog :: Scope
tprog = Scope bprog
