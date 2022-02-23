module IR.Bytecode
    ( Bytecode(..), cons
    , Chunk(..)
    , StkTyp(..)
    , ByteInst(..)
    , emptyChunk
    ) where

import Types (TypeSig(..))
import IR.AST (Builtin(..), ipp)
import Dict (Dict)
import qualified Dict (insert)

newtype Bytecode = Bytecode
    { dict :: Dict String Chunk
    }
    deriving Eq

instance Show Bytecode where
    show (Bytecode ds) = "Bytecode " ++ show ds

cons :: (String, Chunk) -> Bytecode -> Bytecode
cons a (Bytecode as) = Bytecode $ uncurry Dict.insert a as

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
emptyChunk = Chunk (Tfunc [] []) [] $ Bytecode []

data StkTyp
    = I64 Int
    | Quote TypeSig [ByteInst]
    deriving Eq

instance Show StkTyp where
    show (I64      x) = show x
    show (Quote t is) = "[ " ++ ipp is ++ "]::" ++ show t

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