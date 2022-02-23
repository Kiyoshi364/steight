module IR.AST
    ( AST(..)
    , Builtin(..)
    , Inst(..)
    , ipp
    ) where

import Types (TypeSig(..))
import Dict (Dict)

data AST = AST
    { dict :: Dict String (Maybe TypeSig, [Inst])
    }

instance Show AST where show (AST ds) = "AST " ++ show ds

ipp :: Show a => [a] -> String
ipp = foldr (\ i s -> show i ++ " " ++ s) ""

data Builtin
    = Add
    | Sub
    | Swap
    | Rot
    | Dup
    | Drop
    | Print
    | Halt
    | Apply
    deriving Eq

instance Show Builtin where
    show (Add   ) = "+"
    show (Sub   ) = "-"
    show (Swap  ) = "~"
    show (Rot   ) = "rot"
    show (Dup   ) = ":"
    show (Drop  ) = "."
    show (Print ) = "print"
    show (Apply ) = "$"
    show (Halt  ) = "halt"

data Inst
    = Push Int
    | Builtin Builtin
    | PQuote [Inst]
    | Doblk [Inst]
    | Nameblk String [Inst]
    | Typblk TypeSig [Inst]
    | NameTypblk String TypeSig [Inst]
    | Identifier String
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Builtin b) = show b
    show (PQuote is) = "[ " ++ ipp is ++ "]"
    show (Doblk  is) = "do " ++ ipp is ++ "end"
    show (Typblk typ is) = "do <" ++ show typ ++ "> " ++
        ipp is ++ "end"
    show (Nameblk name is) = "block " ++ name ++ " " ++ ipp is ++ "end"
    show (NameTypblk name typ is) = "block " ++ name ++ " <" ++
        show typ ++ "> " ++ ipp is ++ "end"
    show (Identifier ref) = "{" ++ ref ++ "}"
