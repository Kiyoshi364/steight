module Types.UserTypeUtils
    ( userType2destructorType
    , userType2constructors
    ) where

import qualified IR.Identifier as Id (Type, Constructor)
import Types.TypeDef (TypeSig(..), ConstT(User))
import Types.UserType (UserType(UserType), UserCase(UserCase))
import IR.Token (Loc)

import qualified Dict (walkAntiInsertOrder)

-- TODO: upgrade with UserCase
case_ins :: UserCase -> [TypeSig]
case_ins c = const [] c

userType2destructorType :: Id.Type -> UserType -> TypeSig
userType2destructorType name (UserType _ ucs) =
    let
        funs :: [TypeSig]
        funs = Dict.walkAntiInsertOrder
            (\ _ c -> Tfunc (case_ins c) [ Tmany (0,0) ]) ucs
        ins :: [TypeSig]
        ins = funs ++ [ Tconst $ User $ name ]
    in Tfunc ins [ Tmany (0,0) ]

userType2constructors ::
    Id.Type -> UserType -> [(Loc, Id.Constructor, TypeSig)]
userType2constructors name (UserType _ cs) =
    Dict.walkAntiInsertOrder
        (\ c_name c@(UserCase l) -> (
            l,
            c_name,
            Tfunc (case_ins c) [ Tconst $ User $ name]
        )) cs
