module IR.Identifier
    ( Identifier(..)
    , Normal(..), Type, Constructor, Destructor, Builtin
    , mk_normal, mk_type, mk_constructor, mk_destructor, mk_builtin
    , fromNormal, fromType, fromConstructor
    , fromDestructor, fromBuiltin
    ) where

import Parsing.Lexer
    (isUp, isDown, isIntro, isElim, isBuiltin, isSymbol)

data Identifier
    = INormal      Normal
    | IType        Type
    | IConstructor Constructor
    | IDestructor  Destructor
    | IBuiltin     Builtin
    deriving Eq

instance Show Identifier where
    show (INormal      i) = "Normal "      ++ show i
    show (IType        i) = "Type "        ++ show i
    show (IConstructor i) = "Constructor " ++ show i
    show (IDestructor  i) = "Destructor "  ++ show i
    show (IBuiltin     i) = "Builtin "     ++ show i

newtype Normal      = Normal      String deriving (Eq)
newtype Type        = Type        String deriving (Eq)
newtype Constructor = Constructor String deriving (Eq)
newtype Destructor  = Destructor  String deriving (Eq)
newtype Builtin     = Builtin     String deriving (Eq)

instance Show Normal where
    show (Normal s) = show s

instance Show Type where
    show (Type s) = show s

instance Show Constructor where
    show (Constructor s) = show $ "@" ++ s

instance Show Destructor where
    show (Destructor s) = show $ "!" ++ s

instance Show Builtin where
    show (Builtin s) = show $ "#" ++ s

mk_normal :: String -> Normal
mk_normal name@(x:_)
    | isDown x || isSymbol x = Normal name
    | otherwise = error $ "IR.Identifier.mk_normal: invalid Normal `"
mk_normal []    = error $ "IR.Identifier.mk_normal: invalid Normal `[]`"

fromNormal :: String -> Identifier
fromNormal = INormal . mk_normal

mk_type :: String -> Type
mk_type name@(x:_)
    | isUp x    = Type name
    | otherwise = error $ "IR.Identifier.mk_type: invalid Type `"
        ++ name ++ "`"
mk_type []     = error $ "IR.Identifier.mk_type: invalid Type `[]`"

fromType :: String -> Identifier
fromType = IType . mk_type

mk_constructor :: String -> Constructor
mk_constructor name@(x:xs)
    | isIntro x    = Constructor xs
    | otherwise    = error $ "IR.Identifier.mk_constructor: invalid Constructor `"
        ++ name ++ "`"
mk_constructor [] = error $ "IR.Identifier.mk_constructor: invalid Constructor `[]`"

fromConstructor :: String -> Identifier
fromConstructor = IConstructor . mk_constructor

mk_destructor :: String -> Destructor
mk_destructor name@(x:xs)
    | isElim x    = Destructor xs
    | otherwise   = error $ "IR.Identifier.mk_destructor: invalid Destructor `"
        ++ name ++ "`"
mk_destructor [] = error $ "IR.Identifier.mk_destructor: invalid Destructor `[]`"

fromDestructor :: String -> Identifier
fromDestructor = IDestructor . mk_destructor

mk_builtin :: String -> Builtin
mk_builtin name@(x:xs)
    | isBuiltin x = Builtin xs
    | otherwise   = error $ "IR.Identifier.mk_builtin: invalid Builtin `"
        ++ name ++ "`"
mk_builtin []    = error $ "IR.Identifier.mk_builtin: invalid Builtin `[]`"

fromBuiltin :: String -> Identifier
fromBuiltin = IBuiltin . mk_builtin
