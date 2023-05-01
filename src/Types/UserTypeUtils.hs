module Types.UserTypeUtils
    ( userType2destructorType
    , userType2constructors
    ) where

import qualified IR.Identifier as Id (Type, Constructor)
import Types.TypeDef (TypeSig(..), ConstT(User))
import Types.UserType (UserType(UserType), UserCase(UserCase))
import IR.Token (Loc)

-- TODO: upgrade with UserCase
case_ins :: UserCase -> [TypeSig]
case_ins c = const [] c

userType2destructorType :: Id.Type -> UserType -> TypeSig
userType2destructorType name (UserType _ ucs) =
    do_userType2destructorType [Tconst $ User $ name] ucs

do_userType2destructorType :: [TypeSig] -> [UserCase] -> TypeSig
do_userType2destructorType is    []  = Tfunc is [ Tmany (0,0) ]
do_userType2destructorType is (c:cs) =
    do_userType2destructorType
        (Tfunc (case_ins c) [ Tmany (0,0) ] : is)
        cs

userType2constructors ::
    Id.Type -> UserType -> [(Loc, Id.Constructor, TypeSig)]
userType2constructors name (UserType _ cs) =
    let
        join :: UserCase -> [(Loc, Id.Constructor, TypeSig)] -> [(Loc, Id.Constructor, TypeSig)]
        join cas@(UserCase l s) =
            (:) (l, s,
                Tfunc (case_ins cas) [Tconst $ User $ name]
            )
    in foldr join [] cs
