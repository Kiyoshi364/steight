module Simulation where

import Inst

data State a = State
    { ip     :: Int
    , stack  :: [a]
    , out    :: String
    , code   :: Program
    } deriving Show

begin :: Program -> State a
begin = State 0 [] []

simulate :: Program -> IO (State Int)
simulate code = do
    (s, ()) <- return $ loop step $ begin code
    putStrLn "out:"
    putStr $ out s
    return s

loop :: (a -> Either b a) -> a -> (a, b)
loop f x = case f x of
        Right x' -> loop f x'
        Left  b  -> (x, b)

step :: State Int -> Either () (State Int)
step s@(State ip st out code) =
    case getInst ip code of
        Push    x -> Right $ state{ stack = x:st }
        Swap      -> Right $ swap  st
        Dup       -> Right $ dup   st
        Drop      -> Right $ drop  st
        Print     -> Right $ print st out
        Halt      -> Left  ()
        Builtin b -> case b of
            Add -> Right $ add st
            Sub -> Right $ sub st
    where state = s{ ip = (ip+1) }
          swap (b:a:xs)     = state{ stack =   a:b:xs }
          dup  (  a:xs)     = state{ stack =   a:a:xs }
          drop (  a:xs)     = state{ stack =       xs }
          print(  a:xs) out = state{ stack =       xs, out = out++show a++"\n" }
          add  (b:a:xs)     = state{ stack = (a+b):xs }
          sub  (b:a:xs)     = state{ stack = (a-b):xs }

getInst :: Eq a => a -> [(a, b)] -> b
getInst ip = snd . head . filter ((ip==) . fst)
