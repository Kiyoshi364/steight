module Simulation
    ( simulateIO, simulate
    ) where

import IR.Identifier (fromNormal)
import IR.Bytecode
    (Bytecode(..), ByteEntry(..)
    , Chunk(..), StkTyp(..), Builtin(..), ByteInst(..))
import Types (UserType(..), UserCase(..))
import Utils (fork, loop)
import Dict (find)

import qualified Data.List as List (find)

data State a = State
    { stack  :: [a]
    , out    :: String
    , prog   :: Bytecode
    , code   :: [ByteInst]
    } deriving Show

begin :: Bytecode -> State a
begin = fork (State [] []) id $
    maybe [] (\ be -> case be of
        ByteChunk c -> insts c
        _ -> error $ "Simulation.begin: unreachable: " ++ show be )
    . find (fromNormal "main") . dict

simulateIO :: Bytecode -> IO (State StkTyp)
simulateIO scp = do
    (s, rt_err) <- return $ simulate $ begin scp
    either (putStrLn . ("Runtime error: "++)) return rt_err
    if stack s == [] then return () else
        putStrLn "stack:" >> putStrLn (show $ stack s)
    putStrLn "out:"
    putStr $ out s
    return s

simulate :: State StkTyp -> (State StkTyp, Either String ())
simulate = loop step

step :: State StkTyp -> Either (Either String ()) (State StkTyp)
step s@(State _  _  _  []   ) = step s{ code = [Builtin Halt] }
step s@(State st ot p (i:is)) =
    let state = s{ code = is }
        add  (I64 b:I64 a:xs) = state{ stack = I64 (a+b):xs }
        add        _  = undefined
        sub  (I64 b:I64 a:xs) = state{ stack = I64 (a-b):xs }
        sub        _  = undefined
        swap (b:a:xs) = state{ stack =   a:b:xs }
        swap       _  = undefined
        rot (c:b:a:xs) = state{ stack = a:c:b:xs }
        rot        _  = undefined
        dup  (  a:xs) = state{ stack =   a:a:xs }
        dup        _  = undefined
        drpp (  _:xs) = state{ stack =       xs }
        drpp       _  = undefined
        prnt (  a:xs) = state{
                    stack = xs, out = ot++show a++"\n" }
        prnt       _  = undefined
    in case i of
        Push    x -> Right $ state{ stack = x:st }
        Builtin b -> case b of
            Add     -> Right $ add st
            Sub     -> Right $ sub st
            Swap    -> Right $ swap  st
            Rot     -> Right $ rot   st
            Dup     -> Right $ dup   st
            Drop    -> Right $ drpp  st
            Print   -> Right $ prnt  st
            Halt    -> Left  $ Right ()
            Apply   -> case st of
                (Quote _ iis):xs -> let
                    s2 = fst $ simulate s{ stack = xs, code = iis }
                    in Right $ s2{ code = is }
                _                -> undefined
        Chk     b -> let
            iis = insts b
            s2 = fst $ simulate s{ code = iis }
            in Right $ s2{ code = is }
        ChkCall r -> case find r $ dict p of
            Nothing                 -> Left $ Left $
                "Could not find block `" ++ show r ++ "`"
            Just (ByteChunk chk   ) -> let
                iis = insts chk
                (s2, err_hlt) = loop step s{ code = iis }
                in case err_hlt of
                    Left err -> Left $ Left err
                    Right () -> Right $ s2{ code = is }
            Just (ByteTypeDecl typ) -> error (
                    "NOT IMPLEMENTED: Simulation.step.ChkCall.ByteTypeDecl"
                ) typ
        Construct ut@(UserType _ ucs) name ->
            case List.find (\ (UserCase _ n) -> n == name) ucs of
                Nothing -> error $ "Case `" ++ show name ++ "` is not in type `"
                    ++ show ut ++ "`"
                Just _  -> let
                        n = 0
                        udata = UserData ut name $ take n st
                        st2 = udata : drop n st
                        s2 = state{ stack = st2 }
                    in Right $ s2
        Destruct (UserType _ ucs) -> let
                st2 = drop (length ucs) st
                (UserData _ name ust) = head st2
                (Just (Quote _ iis)) = fmap snd
                    . List.find
                        ((\ (UserCase _ n) -> n == name) . fst)
                        $ zip ucs st
                s2 = fst $ simulate s{ stack = ust ++ tail st2, code = iis }
            in Right $ s2{ code = is }
