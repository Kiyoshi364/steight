module IR.Bytecode
    ( Bytecode(..)
    , ByteEntry(..), ByteDict
    , Chunk(..)
    , StkTyp(..)
    , Builtin(..)
    , ByteInst(..)
    , emptyBytecode, cons, emptyChunk
    , fromBuiltin
    ) where

import Types (TypeSig(..), UserType)
import IR.Identifier (Identifier)
import qualified IR.Identifier as Id (Constructor)
import IR.Token (Loc, emptyLoc)
import IR.AST (ipp)
import qualified IR.AST as AST (Builtin(..))
import Dict (Dict)
import qualified Dict as D (insert, emptyDict)

data ByteEntry
    = ByteChunk Chunk
    | ByteTypeDecl UserType
    deriving (Eq, Show)

type ByteDict  = Dict Identifier ByteEntry

newtype Bytecode = Bytecode
    { dict :: ByteDict
    } deriving Eq

instance Show Bytecode where
    show (Bytecode ds) = "Bytecode " ++ show ds

emptyBytecode :: Bytecode
emptyBytecode = Bytecode D.emptyDict

cons :: (Identifier, ByteEntry) -> Bytecode -> Bytecode
cons a (Bytecode as) = Bytecode $ uncurry D.insert a as

data Chunk = Chunk
    { bloc  :: Loc
    , typT  :: TypeSig
    , insts :: [ByteInst]
    , scope :: Bytecode
    } deriving Eq

instance Show Chunk where
    show (Chunk l _   [] scp) = show l ++ ": scope " ++ show scp
    show (Chunk l typ is (Bytecode [])) =
        show l ++ ": do " ++ show typ ++ " " ++ ipp is ++ "end"
    show (Chunk l typ is scp) = show l ++ ": scope " ++ show scp ++
        " do " ++ show typ ++ " " ++ ipp is ++ "end"

emptyChunk :: Chunk
emptyChunk = Chunk emptyLoc (Tfunc [] []) [] $ emptyBytecode

data StkTyp
    = I64 Int
    | Quote TypeSig [ByteInst]
    | Type TypeSig
    | UserData UserType Id.Constructor [StkTyp]
    deriving Eq

instance Show StkTyp where
    show (I64      x) = show x
    show (Quote t is) = "[ " ++ ipp is ++ "]::" ++ show t
    show (Type  t   ) = show t
    show (UserData  ut name ds) =
        "(" ++ show ut ++ ")." ++ show name ++ " " ++ show ds

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

fromBuiltin :: AST.Builtin -> Builtin
fromBuiltin (AST.Add  ) = Add
fromBuiltin (AST.Sub  ) = Sub
fromBuiltin (AST.Swap ) = Swap
fromBuiltin (AST.Rot  ) = Rot
fromBuiltin (AST.Dup  ) = Dup
fromBuiltin (AST.Drop ) = Drop
fromBuiltin (AST.Print) = Print
fromBuiltin (AST.Apply) = Apply
fromBuiltin (AST.Halt ) = Halt
fromBuiltin (AST.I64b ) = error "IR.Bytecode.fromBuiltin: unhandled case I64b"

data ByteInst
    = Push StkTyp
    | Builtin Builtin
    | Chk Chunk
    | ChkCall Identifier
    | Construct UserType Id.Constructor
    | Destruct UserType
    deriving Eq

instance Show ByteInst where
    show (Push        p) = show p
    show (Builtin     b) = show b
    show (Chk         b) = show b
    show (ChkCall     r) = "{" ++ show r ++ "}"
    show (Construct t c) = "@(" ++ show t ++ ")." ++ show c
    show (Destruct    t) = "!" ++ show t
