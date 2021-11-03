module IR
    ( Program(..)
    , Block(..)
    , IRInst(..)
    , emptyBlock
    ) where

import Types (TypeSig(..))
import Inst (Inst, Builtin(..), ipp, instTyp)
import qualified Inst
import Utils (fork)
import Dict (Dict)

data Program = Program
    { dict :: Dict String Block
    }

instance Show Program where show (Program ds) = "Program " ++ show ds

data Block = Block
    { typT  :: TypeSig
    , insts :: [IRInst]
    }
    deriving Eq

instance Show Block where
    show (Block typ is)
        = "do " ++ show typ ++ " " ++
        ipp is ++ "end"

emptyBlock :: Block
emptyBlock = Block (Tfunc [] []) []

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
