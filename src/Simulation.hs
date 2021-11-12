module Simulation
    ( simulateIO, simulate
    ) where

import Inst (Builtin(..))
import IR (Scope(..), Block(..), StkTyp(..), IRInst(..))
import Utils (fork, loop)
import Dict (find)

data State a = State
    { stack  :: [a]
    , out    :: String
    , prog   :: Scope
    , code   :: [IRInst]
    } deriving Show

begin :: Scope -> State a
begin = fork (State [] []) id $ maybe [] insts . find "main" . dict

simulateIO :: Scope -> IO (State StkTyp)
simulateIO scp = do
    (s, rt_err) <- return $ simulate scp
    either (putStrLn . ("Runtime error: "++)) return rt_err
    if stack s == [] then return () else
        putStrLn "stack:" >> putStrLn (show $ stack s)
    putStrLn "out:"
    putStr $ out s
    return s

simulate :: Scope -> (State StkTyp, Either String ())
simulate = loop step . begin

step :: State StkTyp -> Either (Either String ()) (State StkTyp)
step s@(State _  _  _  []   ) = step s{ code = [Builtin Halt] }
step s@(State st ot p (i:is)) =
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
        add  (I64 b:I64 a:xs) = state{ stack = I64 (a+b):xs }
        add        _  = undefined
        sub  (I64 b:I64 a:xs) = state{ stack = I64 (a-b):xs }
        sub        _  = undefined
    in case i of
        Push    x -> Right $ state{ stack = x:st }
        Builtin b -> case b of
            Add     -> Right $ add st
            Sub     -> Right $ sub st
            Swap    -> Right $ swap  st
            Dup     -> Right $ dup   st
            Drop    -> Right $ drpp  st
            Print   -> Right $ prnt  st
            Halt    -> Left  $ Right ()
        Blk     b -> let
            iis = insts b
            s2 = fst $ loop step s{ code = iis }
            in Right $ s2{ code = is }
        BlkCall r -> case find r $ dict p of
            Nothing  -> Left $ Left $ "Could not find block `"
                ++ r ++ "`"
            Just blk -> let
                iis = insts blk
                (s2, err_hlt) = loop step s{ code = iis }
                in case err_hlt of
                    Left err -> Left $ Left err
                    Right () -> Right $ s2{ code = is }
