module Main
    ( main
    , iprog, bprog, tprog
    ) where

import Parsing.Lexer (tokenize)
import Parsing.Parser (parse)
import IR.AST (Inst, Inst(..), Builtin(..), AST(..))
import IR.Bytecode (Bytecode(Bytecode), Chunk(..), emptyChunk)
import Typecheck (typecheckIO, typecheck)
import Simulation (simulateIO)
import Dict (Dict)
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    path <- if args == [] then putStrLn "empty args" >> return ""
                           else return $ head args
    input <- readFile path
    -- putStrLn "Args:"
    -- mapM putStrLn args
    -- putStr "reading: "
    -- line <- getLine
    -- putStrLn $ (++) "input: " $ show input
    tokens <- return $ tokenize input
    parsed <- return $ parse tokens
    -- putStrLn $ (++) "parsed: " $ either ("error: "++) (pp) $ P.value parsed
    -- putStrLn $ (++) "leftover: " $ show $ P.input parsed
    prog <- case parsed of
        Left  errs -> putStrLn (foldMap
            (\ (loc, err) -> "\n" ++ show loc ++ ": " ++ err) errs)
            >> return (AST [("main", (Nothing, []))])
        Right  r   -> return r
    putStrLn $ show prog
    putStrLn ""
    (p', ok) <- typecheckIO prog
    putStrLn $ show p'
    _ <- if ok then putStrLn "=== simulation ===" >> simulateIO p'
          else simulateIO $ Bytecode [("main", emptyChunk)]
    return ()

iprog :: [Inst]
iprog = [
    Block Nothing Nothing [Push 2, Push 1, Builtin Add],
    Builtin Dup, Builtin Print, Push 3,
    Builtin Swap, Builtin Sub, Builtin Print
    ]

bprog :: Dict String Chunk
bprog = (\ (_, b, _) -> b) $ typecheck $ AST [("main", (Nothing, iprog))]

tprog :: Bytecode
tprog = Bytecode bprog
