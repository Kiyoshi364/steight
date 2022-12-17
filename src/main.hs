module Main
    ( main
    , iprog, bprog, tprog
    ) where

import Parsing.Lexer (tokenize)
import Parsing.Parser (parse)
import IR.Token (emptyLoc)
import IR.AST (AST(..), ASTEntry(ASTBlock)
    , Inst(..), Instruction(..), Builtin(..))
import IR.Bytecode (Bytecode(Bytecode), ByteEntry(..), emptyChunk)
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
            >> return (AST [("main", ASTBlock emptyLoc Nothing [])])
        Right  r   -> return r
    putStrLn $ show prog
    putStrLn ""
    (p', ok) <- typecheckIO prog
    putStrLn $ show p'
    _ <- if ok then putStrLn "\n=== simulation ===" >> simulateIO p'
          else simulateIO $ Bytecode [("main", ByteChunk emptyChunk)]
    return ()

iprog :: [Inst]
iprog = [
    toI (Block Nothing Nothing) [toI Push 2, toI Push 1, toI Builtin Add],
    toI Builtin Dup, toI Builtin Print, toI Push 3,
    toI Builtin Swap, toI Builtin Sub, toI Builtin Print
    ]

toI :: (a -> Instruction) -> a -> Inst
toI f x = Inst emptyLoc $ f x

bprog :: Dict String ByteEntry
bprog = (\ (_, b, _) -> b) $ typecheck
    $ AST [("main", ASTBlock emptyLoc Nothing iprog)]

tprog :: Bytecode
tprog = Bytecode bprog
