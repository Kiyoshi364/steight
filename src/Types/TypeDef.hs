module Types.TypeDef
    ( IVar, IMany, ConstT(..)
    , TypeSig(..)
    ) where

type IVar   = Int
type IMany  = (,) Int Int

data ConstT
    = I64
    | Type
    deriving (Show, Eq)

data TypeSig
    = Tconst ConstT
    | Tfunc [TypeSig] [TypeSig]
    | Tvar IVar
    | Tmany IMany
    deriving (Eq)

instance Show TypeSig where
    show (Tconst   t ) = show t
    show (Tfunc [] []) = "()"
    show (Tfunc [] o ) = "( " ++ revcat o ++ ")"
    show (Tfunc i  o ) = "( " ++ revcat i ++ "-- " ++ revcat o ++ ")"
    show (Tvar     n ) = "'" ++ show n
    show (Tmany (n,0)) = "%" ++ show n
    show (Tmany    n ) = "%" ++ show (fst n) ++ "'" ++ show (snd n)

revcat :: Show a => [a] -> String
revcat = foldr (\t s -> s ++ show t ++ " ") ""
