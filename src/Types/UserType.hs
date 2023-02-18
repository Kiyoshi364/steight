module Types.UserType
    ( UserType(..)
    , UserCase(..)
    ) where

import IR.Token (Loc)

data UserType = UserType Loc [UserCase]
    deriving (Eq, Show)

data UserCase = UserCase Loc String
    deriving (Eq, Show)
