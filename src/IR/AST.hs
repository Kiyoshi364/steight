module IR.AST
    ( AST(..)
    , ASTEntry(..), ASTDict
    , Builtin(..)
    , AVar(..)
    , TypeLit(..)
    , CaseDecl(..)
    , Inst(..)
    , Instruction(..)
    , emptyAST, cons, astEntryLoc
    , ipp
    , builtinTyp
    ) where

import IR.Identifier (Identifier)
import qualified IR.Identifier as Id (Normal, Type)
import IR.Token (Loc)
import Types.TypeDef (TypeSig(..), ConstT(..))
import Dict (Dict)
import qualified Dict as D (emptyDict, insert)

data ASTEntry
    = ASTBlock Loc (Maybe (Loc, TypeLit)) [Inst]
    | ASTTypeDecl Loc (Loc, TypeLit) [(Loc, CaseDecl)]
    deriving (Eq, Show)

type ASTDict  = Dict Identifier ASTEntry

data AST = AST
    { dict :: ASTDict
    }

instance Show AST where show (AST ds) = "AST " ++ show ds

emptyAST :: AST
emptyAST = AST D.emptyDict

cons :: Identifier -> ASTEntry -> AST -> AST
cons k v (AST d) = AST $ D.insert k v d

astEntryLoc :: ASTEntry -> Loc
astEntryLoc (ASTBlock    l _ _) = l
astEntryLoc (ASTTypeDecl l _ _) = l

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

data TypeLit = TypeLit [(Loc, Either AVar Inst)] [(Loc, Either AVar Inst)]
    deriving Eq

instance Show TypeLit where
    show (TypeLit [] []) = "()"
    show (TypeLit [] o ) = "( " ++ revcatTL o ++ ")"
    show (TypeLit i  o ) = "( " ++ revcatTL i ++ "-- " ++ revcatTL o ++ ")"

revcatTL :: [(Loc, Either AVar Inst)] -> String
revcatTL = foldr (\t s -> s ++ f t ++ " ") ""
  where
    f :: (Loc, Either AVar Inst) -> String
    f (l, eai) = show l ++ ":" ++ either show show eai

data CaseDecl = CaseDecl (Loc, Id.Normal) (Loc, TypeLit)
    deriving Eq

instance Show CaseDecl where
    show (CaseDecl l_name l_typ) = "case " ++ show l_name ++ " " ++ show l_typ

data Instruction
    = Push Int
    | Builtin Builtin
    | PQuote [Inst]
    | PType TypeLit
    | Block (Maybe (Loc, Id.Normal)) (Maybe (Loc, TypeLit)) [Inst]
    | TypeDecl (Loc, Id.Type) (Loc, TypeLit) [(Loc, CaseDecl)]
    | Identifier Identifier
    deriving Eq

instance Show Instruction where
    show (Push    x) = show x
    show (Builtin b) = show b
    show (PQuote is) = "[ " ++ ipp is ++ "]"
    show (PType typ) = show typ
    show (Block m_name m_typ is) =
        maybe "do" (("block "++) . show) m_name ++
        maybe " " (\ t -> " <" ++ show t ++ "> ") m_typ ++
        ipp is ++ "end"
    show (TypeDecl l_name l_typ cs) = "type " ++ show l_name ++
        " " ++ show l_typ ++ " " ++ ipp cs
    show (Identifier ref) = "{" ++ show ref ++ "}"

data Inst = Inst
    { iloc  :: Loc
    , instr :: Instruction
    } deriving Eq

instance Show Inst where
    show (Inst l i) = "(" ++ show l ++ "," ++ show i ++ ")"

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
