module IR
    ( Scope(..)
    , Block(..)
    , IRInst(..)
    , emptyBlock
    ) where

import Types (TypeSig(..))
import Inst (Builtin(..), ipp)
import Dict (Dict)

data Scope = Scope
    { dict :: Dict String Block
    }

instance Show Scope where show (Scope ds) = "Scope " ++ show ds

data Block = Block
    { typT  :: TypeSig
    , insts :: [IRInst]
    , scope :: Dict String Block
    }
    deriving Eq

instance Show Block where
    show (Block  _  [] scp) = "scope " ++ show scp
    show (Block typ is [] ) =
        " do " ++ show typ ++ " " ++ ipp is ++ "end"
    show (Block typ is scp) = "scope " ++ show scp ++
        " do " ++ show typ ++ " " ++ ipp is ++ "end"

emptyBlock :: Block
emptyBlock = Block (Tfunc [] []) [] []

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
