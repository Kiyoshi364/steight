module Types.UserType
    ( UserType(..)
    , UserCase(..)
    ) where

import qualified IR.Identifier as Id (Normal)
import IR.Token (Loc)

data UserType = UserType Loc [UserCase]
    deriving (Eq, Show)

data UserCase = UserCase Loc Id.Normal
    deriving (Eq, Show)
