module Main (main) where

import qualified Parser as P
import Inst (Inst, Inst(Halt), lexer, Program)
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
        Left  l -> putStrLn l >> return [(0, Halt)]
        Right r ->               return r
    ok <- typecheck prog
    if ok then putStrLn "=== simulation ===" >> simulate prog
          else return $ begin [(0, Halt)]
    return ()

wsP :: P.Parser [String]
wsP = many $ P.wsP <|> P.lfP

pp :: Program -> String
pp = foldr (\ t -> ((show (snd t) ++ ", ") ++)) ""
