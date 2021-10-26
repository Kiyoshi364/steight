module Simulation where

import Inst

data State a = State
    { stack  :: [a]
    , out    :: String
    , prog   :: Program
    } deriving Show

begin :: Program -> State a
begin = State [] []

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

step s@(State st out p@(Program code)) =
    case code of
        []   -> Left ()
        i:is ->
            let state = s{ prog = p{ code = is } }
                swap (b:a:xs)     = state{ stack =   a:b:xs }
                dup  (  a:xs)     = state{ stack =   a:a:xs }
                drop (  a:xs)     = state{ stack =       xs }
                print(  a:xs) out = state{
                                stack =       xs, out = out++show a++"\n" }
                add  (b:a:xs)     = state{ stack = (a+b):xs }
                sub  (b:a:xs)     = state{ stack = (a-b):xs }
            in case i of
                Push    x -> Right $ state{ stack = x:st }
                Swap      -> Right $ swap  st
                Dup       -> Right $ dup   st
                Drop      -> Right $ drop  st
                Print     -> Right $ print st out
                Halt      -> Left  ()
                Builtin b -> case b of
                    Add -> Right $ add st
                    Sub -> Right $ sub st

-- step :: State Int -> Either () (State Int)
-- step s@(State ip st out p@(Program i:is)) =
--     case getInst ip code of
--         Push    x -> Right $ state{ stack = x:st }
--         Swap      -> Right $ swap  st
--         Dup       -> Right $ dup   st
--         Drop      -> Right $ drop  st
--         Print     -> Right $ print st out
--         Halt      -> Left  ()
--         Builtin b -> case b of
--             Add -> Right $ add st
--             Sub -> Right $ sub st
--     where state = s{ ip = (ip+1) }
--           swap (b:a:xs)     = state{ stack =   a:b:xs }
--           dup  (  a:xs)     = state{ stack =   a:a:xs }
--           drop (  a:xs)     = state{ stack =       xs }
--           print(  a:xs) out = state{ stack =       xs, out = out++show a++"\n" }
--           add  (b:a:xs)     = state{ stack = (a+b):xs }
--           sub  (b:a:xs)     = state{ stack = (a-b):xs }

getInst :: Eq a => a -> [(a, b)] -> b
getInst ip = snd . head . filter ((ip==) . fst)
