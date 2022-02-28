module IR.AST
    ( AST(..)
    , Builtin(..)
    , TypeLit(..)
    , Inst(..)
    , emptyAST, cons
    , ipp
    ) where

import Dict (Dict)
import qualified Dict as D (emptyDict, insert)

data AST = AST
    { dict :: Dict String (Maybe TypeLit, [Inst])
    }

instance Show AST where show (AST ds) = "AST " ++ show ds

emptyAST :: AST
emptyAST = AST D.emptyDict

cons :: String -> (Maybe TypeLit, [Inst]) -> AST -> AST
cons k v (AST d) = AST $ D.insert k v d

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

data TypeLit = TypeLit [Inst] [Inst]
    deriving Eq

instance Show TypeLit where
    show (TypeLit [] []) = "()"
    show (TypeLit [] o ) = "( " ++ revcat o ++ ")"
    show (TypeLit i  o ) = "( " ++ revcat i ++ "-- " ++ revcat o ++ ")"

revcat :: Show a => [a] -> String
revcat = foldr (\t s -> s ++ show t ++ " ") ""

data Inst
    = Push Int
    | Builtin Builtin
    | PQuote [Inst]
    | PType  TypeLit
    | Doblk [Inst]
    | Nameblk String [Inst]
    | Typblk TypeLit [Inst]
    | NameTypblk String TypeLit [Inst]
    | Identifier String
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Builtin b) = show b
    show (PQuote is) = "[ " ++ ipp is ++ "]"
    show (PType typ) = show typ
    show (Doblk  is) = "do " ++ ipp is ++ "end"
    show (Typblk typ is) = "do <" ++ show typ ++ "> " ++
        ipp is ++ "end"
    show (Nameblk name is) = "block " ++ name ++ " " ++ ipp is ++ "end"
    show (NameTypblk name typ is) = "block " ++ name ++ " <" ++
        show typ ++ "> " ++ ipp is ++ "end"
    show (Identifier ref) = "{" ++ ref ++ "}"
