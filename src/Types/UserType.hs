module Types.UserType
    ( UserType(..)
    , UserCase(..)
    , DictUserCase
    ) where

import qualified IR.Identifier as Id (Constructor)
import IR.Token (Loc)
import Dict (Dict)

data UserType = UserType Loc DictUserCase
    deriving (Eq, Show)

data UserCase = UserCase Loc
    deriving (Eq, Show)

type DictUserCase = Dict Id.Constructor UserCase
