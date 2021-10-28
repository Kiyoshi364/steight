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
step s@(State st out p@(Program code)) =
    case code of
        []   -> step s{ prog = p{ code = [Halt] } }
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
                Halt      -> Left  $ Right ()
                Builtin b -> case b of
                    Add -> Right $ add st
                    Sub -> Right $ sub st
                Doblk iis -> Left  $
                    Left "Atempting to step through a `do-block`"
                Typblk ips ots iis -> Left  $
                    Left "Atempting to step through a `typed-do-block`"
                Blk     b -> let
                    iis = insts b
                    s2 = fst $ loop step s{ prog = p{ code = iis } }
                    in Right $ s2{ prog = p{ code = is } }
