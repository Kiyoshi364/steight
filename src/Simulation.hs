module Simulation (simulate) where

import Inst (Builtin(..))
import IR (Program(..), Block(..), IRInst(..))
import Utils (fork)

data State a = State
    { stack  :: [a]
    , out    :: String
    , prog   :: Program
    , code   :: [IRInst]
    } deriving Show

find :: Eq a => a -> [(a, b)] -> Maybe b
find x  []      = Nothing
find x ((a, b):ps)
    | x == a    = Just b
    | otherwise = find x ps

begin :: Program -> State a
begin = fork (State [] []) id $ maybe [] insts . find "main" . dict

simulate :: Program -> IO (State Int)
simulate code = do
    (s, rt_err) <- return $ loop step $ begin code
    either (putStrLn . ("Runtime error: "++)) return rt_err
    if stack s == [] then return () else
        putStrLn "stack:" >> putStrLn (show $ stack s)
    putStrLn "out:"
    putStr $ out s
    return s

loop :: (a -> Either b a) -> a -> (a, b)
loop f x = case f x of
        Right x' -> loop f x'
        Left  b  -> (x, b)

step :: State Int -> Either (Either String ()) (State Int)
step s@(State st out prog  []   ) = step s{ code = [Halt] }
step s@(State st out prog (i:is)) =
    let state = s{ code = is }
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
        Halt      -> Left  $ Right ()
        Builtin b -> case b of
            Add -> Right $ add st
            Sub -> Right $ sub st
        Blk     b -> let
            iis = insts b
            s2 = fst $ loop step s{ code = iis }
            in Right $ s2{ code = is }
