module Simulation where

import Inst

data State = State
    { ip     :: Int
    , stack  :: [Int]
    , code   :: Program
    } deriving Show

prog :: [(Int, Inst)]
prog = zip [0..] [Push 3, Push 2, Builtin Sub, Push 1, Builtin Add, Halt]

begin :: Program -> State
begin = State 0 []

simulate :: Program -> IO State
simulate code = do
    s <- return $ loop step $ begin code
    putStr "stack: "
    putStrLn $ show $ stack s
    return s

loop :: (a -> Maybe a) -> a -> a
loop f x = case f x of
        Just x' -> loop f x'
        Nothing -> x

step :: State -> Maybe State
step s = if halt then Nothing else Just s{ ip = newIp, stack = newStack }
    where (newIp, newStack, halt) = do_step s

do_step :: State -> (Int, [Int], Bool)
do_step (State ip st code) =
    case getInst ip code of
        Push    x -> (ip+1, x:st, False)
        Halt      -> (ip+1, st, True)
        Builtin b -> case b of
            Add -> (ip+1, add st, False)
            Sub -> (ip+1, sub st, False)
    where add (b:a:xs) = (a+b):xs
          sub (b:a:xs) = (a-b):xs

getInst :: Eq a => a -> [(a, b)] -> b
getInst ip = snd . head . filter ((ip==) . fst)
