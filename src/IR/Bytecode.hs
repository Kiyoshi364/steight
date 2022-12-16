module IR.Bytecode
    ( Bytecode(..)
    , ByteDict
    , Chunk(..)
    , StkTyp(..)
    , Builtin(..)
    , ByteInst(..)
    , emptyBytecode, cons, emptyChunk
    , fromBuiltin
    ) where

import Types (TypeSig(..))
import IR.AST (ipp)
import qualified IR.AST as AST (Builtin(..))
import Dict (Dict)
import qualified Dict as D (insert, emptyDict)

type ByteDict  = Dict String Chunk

newtype Bytecode = Bytecode
    { dict :: ByteDict
    } deriving Eq

instance Show Bytecode where
    show (Bytecode ds) = "Bytecode " ++ show ds

emptyBytecode :: Bytecode
emptyBytecode = Bytecode D.emptyDict

cons :: (String, Chunk) -> Bytecode -> Bytecode
cons a (Bytecode as) = Bytecode $ uncurry D.insert a as

data Chunk = Chunk
    { typT  :: TypeSig
    , insts :: [ByteInst]
    , scope :: Bytecode
    }
    deriving Eq

instance Show Chunk where
    show (Chunk  _  [] scp) = "scope " ++ show scp
    show (Chunk typ is (Bytecode [])) =
        "do " ++ show typ ++ " " ++ ipp is ++ "end"
    show (Chunk typ is scp) = "scope " ++ show scp ++
        " do " ++ show typ ++ " " ++ ipp is ++ "end"

emptyChunk :: Chunk
emptyChunk = Chunk (Tfunc [] []) [] $ emptyBytecode

data StkTyp
    = I64 Int
    | Quote TypeSig [ByteInst]
    | Type TypeSig
    deriving Eq

instance Show StkTyp where
    show (I64      x) = show x
    show (Quote t is) = "[ " ++ ipp is ++ "]::" ++ show t
    show (Type  t   ) = show t

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
    | ChkCall String
    deriving Eq

instance Show ByteInst where
    show (Push    p) = show p
    show (Builtin b) = show b
    show (Chk     b) = show b
    show (ChkCall r) = "{" ++ r ++ "}"
