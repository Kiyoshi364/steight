module IR
    ( Scope(..)
    , Block(..)
    , StkTyp(..)
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
        "do " ++ show typ ++ " " ++ ipp is ++ "end"
    show (Block typ is scp) = "scope " ++ show scp ++
        " do " ++ show typ ++ " " ++ ipp is ++ "end"

emptyBlock :: Block
emptyBlock = Block (Tfunc [] []) [] []

data StkTyp
    = I64 Int
    | Quote TypeSig [IRInst]
    deriving Eq

instance Show StkTyp where
    show (I64      x) = show x
    show (Quote t is) = "[ " ++ ipp is ++ "]::" ++ show t

data IRInst
    = Push StkTyp
    | Builtin Builtin
    | Blk Block
    | BlkCall String
    deriving Eq

instance Show IRInst where
    show (Push    p) = show p
    show (Builtin b) = show b
    show (Blk     b) = show b
    show (BlkCall r) = "{" ++ r ++ "}"
