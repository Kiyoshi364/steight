module IR where

import Inst (Inst, Builtin(..), ipp, tpp, TypeSig, instTyp)
import qualified Inst
import Utils (fork)

data Program = Program
    { dict :: [(String, Block)]
    }
    deriving Show

data Block = Block
    { inpT  :: [TypeSig]
    , outT  :: [TypeSig]
    , insts :: [IRInst]
    }
    deriving Eq

instance Show Block where
    show (Block inp out is)
        = "do " ++ tpp (inp, out) ++ " " ++
        ipp is ++ "end"

blockTyp :: Block -> ([TypeSig], [TypeSig])
blockTyp = fork (,) inpT outT

data IRInst
    = Push Int
    | Swap
    | Dup
    | Drop
    | Print
    | Halt
    | Builtin Builtin
    | Blk Block
    deriving Eq

instance Show IRInst where
    show (Push    x) = show x
    show (Swap     ) = "~"
    show (Dup      ) = ":"
    show (Drop     ) = "."
    show (Print    ) = "print"
    show (Halt     ) = "halt"
    show (Builtin b) = show b
    show (Blk     b) = show b
