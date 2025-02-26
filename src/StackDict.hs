{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module StackDict
    ( Dict
    , emptyDict, insert
    , findPair, find, findPairWith, findWith
    , partPair, part, partPairWith, partWith
    , walk, walkAntiInsertOrder
    ) where

import Utils.FakeStack
import qualified Data.List as L (find, partition)

type Dict k v = [(k, v)]

emptyDict :: Dict k v
emptyDict = []

insert :: Stack (k -> v -> Dict k v -> r) (Dict k v -> r)
insert = stack2 (,) . stack2 (:)

-- findPair :: Eq k => k ->  Dict k v -> Maybe (k, v)
-- findPair = findPairWith . (==)

findPairWith :: Stack
    ((forall r1 . Stack (k -> r1) (Bool -> r1)) -> Dict k v -> r)
    (Maybe (k, v) -> r)
findPairWith =
    -- (undefined
    --     :: Stack
    --         ((forall r1 . Stack (k -> r1) (Bool -> r1)) -> Dict k v -> r)
    --         (Maybe (k, v) -> r)
    --     -> Stack
    --         ((k -> Bool) -> Dict k v -> r)
    --         (Maybe (k, v) -> r)
    -- )
    _
    . stack1 (. fst)
    . stack2 L.find
-- findPairWith = unstack1 . stack1 (. fst) . stack2 L.find
