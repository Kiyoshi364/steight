module IR.AST
    ( AST(..)
    , Builtin(..)
    , AVar(..)
    , TypeLit(..)
    , Inst(..)
    , emptyAST, cons
    , ipp
    , builtinTyp
    ) where

import Types.TypeDef (TypeSig(..), ConstT(..))
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
    | I64b
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
    show (I64b  ) = "I64"

data AVar = Avar Int | Amany Int
    deriving (Show, Eq)

data TypeLit = TypeLit [Either AVar Inst] [Either AVar Inst]
    deriving Eq

instance Show TypeLit where
    show (TypeLit [] []) = "()"
    show (TypeLit [] o ) = "( " ++ revcatTL o ++ ")"
    show (TypeLit i  o ) = "( " ++ revcatTL i ++ "-- " ++ revcatTL o ++ ")"

revcatTL :: [Either AVar Inst] -> String
revcatTL = foldr (\t s -> s ++ f t ++ " ") ""
  where
    f :: Either AVar Inst -> String
    f = either show show

data Inst
    = Push Int
    | Builtin Builtin
    | PQuote [Inst]
    | PType TypeLit
    | Block (Maybe String) (Maybe TypeLit) [Inst]
    | Identifier String
    deriving Eq

instance Show Inst where
    show (Push    x) = show x
    show (Builtin b) = show b
    show (PQuote is) = "[ " ++ ipp is ++ "]"
    show (PType typ) = show typ
    show (Block m_name m_typ is) =
        maybe "do" (("block "++) . show) m_name ++
        maybe " " (\ t -> " <" ++ show t ++ "> ") m_typ ++
        ipp is ++ "end"
    show (Identifier ref) = "{" ++ ref ++ "}"

i64 :: TypeSig
i64 = Tconst I64

ttyp :: TypeSig
ttyp = Tconst Type

tmany :: Int -> TypeSig
tmany i = Tmany (i, 0)

builtinTyp :: Builtin -> TypeSig
builtinTyp b = case b of
    Add     -> Tfunc [ i64   , i64            ] [ i64                    ]
    Sub     -> Tfunc [ i64   , i64            ] [ i64                    ]
    Swap    -> Tfunc [ Tvar 0, Tvar 1         ] [ Tvar 1, Tvar 0         ]
    Rot     -> Tfunc [ Tvar 0, Tvar 1, Tvar 2 ] [ Tvar 2, Tvar 0, Tvar 1 ]
    Dup     -> Tfunc [ Tvar 0                 ] [ Tvar 0, Tvar 0         ]
    Drop    -> Tfunc [ Tvar 0                 ] [                        ]
    Print   -> Tfunc [ Tvar 0                 ] [                        ]
    Apply   -> Tfunc [ Tfunc [tmany 0] [tmany 1], tmany 0 ] [ tmany 1    ]
    Halt    -> Tfunc [                        ] [                        ]
    I64b    -> Tfunc [                        ] [ ttyp                   ]
