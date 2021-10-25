module Typecheck where

import Inst
import Simulation (State(State, ip, stack), begin, getInst)

typecheck :: Program -> IO Bool
typecheck code = do
    merr <- return $ loop step $ begin code
    case merr of
        Just (x, err) -> putStrLn err   >> return False
        Nothing       -> putStrLn "Ok!" >> return True

loop :: (a -> (a, Either Bool b)) -> a -> Maybe (a, b)
loop f x = case f x of
        (x', Right b    ) -> Just (x, b)
        (x', Left  False) -> Nothing
        (x', Left  True ) -> loop f x'

step :: State TypeSig -> (State TypeSig, Either Bool String)
step s@(State ip st _ code) =
    case getInst ip code of
        Push    x -> (state{ stack = I64:st }, Left True )
        Swap      -> swap st
        Dup       -> dup  st
        Drop      -> drop st
        Print     -> drop st
        Halt      -> halt st
        Builtin b -> case b of
            Add -> add st
            Sub -> sub st
    where state = s{ ip = (ip+1) }
          swap (   y:x:xs) = (state{ stack = x:y:xs }, Left  True )
          swap         xs  = (state{ stack =     [] }, Right$"`swap` expected a b, found" ++ show xs)
          dup  (     x:xs) = (state{ stack = x:x:xs }, Left  True )
          dup          xs  = (state{ stack =     [] }, Right$"`dup` expected t, found []")
          drop (     x:xs) = (state{ stack =     xs }, Left  True )
          drop         []  = (state{ stack =     [] }, Right$"`drop` expected t, found []")
          halt         []  = (state{ stack =     [] }, Left  False)
          halt         xs  = (state{ stack =     xs }, Right$ "`End of Block` expected [], found " ++ show xs)
          add (I64:I64:xs) = (state{ stack = I64:xs }, Left  True )
          add          xs  = (state{ stack =     xs }, Right$"`+` expected I64 I64, found " ++ show xs)
          sub (I64:I64:xs) = (state{ stack = I64:xs }, Left  True )
          sub          xs  = (state{ stack =     xs }, Right$"`-` expected I64 I64, found " ++ show xs)
