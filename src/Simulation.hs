module Simulation (simulate, loop) where

import Inst (Builtin(..))
import IR (Scope(..), Block(..), IRInst(..))
import Utils (fork, loop)
import Dict (find)

data State a = State
    { stack  :: [a]
    , out    :: String
    , prog   :: Scope
    , code   :: [IRInst]
    } deriving Show

begin :: Scope -> State a
begin = fork (State [] []) id $ maybe [] insts . find (=="main") . dict

simulate :: Scope -> IO (State Int)
simulate p = do
    (s, rt_err) <- return $ loop step $ begin p
    either (putStrLn . ("Runtime error: "++)) return rt_err
    if stack s == [] then return () else
        putStrLn "stack:" >> putStrLn (show $ stack s)
    putStrLn "out:"
    putStr $ out s
    return s

step :: State Int -> Either (Either String ()) (State Int)
step s@(State _  _  _  []   ) = step s{ code = [Halt] }
step s@(State st ot _ (i:is)) =
    let state = s{ code = is }
        swap (b:a:xs) = state{ stack =   a:b:xs }
        swap       _  = undefined
        dup  (  a:xs) = state{ stack =   a:a:xs }
        dup        _  = undefined
        drpp (  _:xs) = state{ stack =       xs }
        drpp       _  = undefined
        prnt (  a:xs) = state{
                    stack = xs, out = ot++show a++"\n" }
        prnt       _  = undefined
        add  (b:a:xs) = state{ stack = (a+b):xs }
        add        _  = undefined
        sub  (b:a:xs) = state{ stack = (a-b):xs }
        sub        _  = undefined
    in case i of
        Push    x -> Right $ state{ stack = x:st }
        Swap      -> Right $ swap  st
        Dup       -> Right $ dup   st
        Drop      -> Right $ drpp  st
        Print     -> Right $ prnt  st
        Halt      -> Left  $ Right ()
        Builtin b -> case b of
            Add -> Right $ add st
            Sub -> Right $ sub st
        Blk     b -> let
            iis = insts b
            s2 = fst $ loop step s{ code = iis }
            in Right $ s2{ code = is }
