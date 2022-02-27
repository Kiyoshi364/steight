module Dict
    ( Dict
    , emptyDict, insert
    , findPair, find, findPairWith, findWith
    , partPair, part, partPairWith, partWith
    ) where

import Utils (onPair, dup)
import qualified Data.List as L (find, partition)

type Dict k v = [(k, v)]

emptyDict :: Dict k v
emptyDict = []

insert :: k -> v -> Dict k v -> Dict k v
insert k v d = (k, v):d

findPair :: Eq k => k ->  Dict k v -> Maybe (k, v)
findPair = findPairWith . (==)

findPairWith :: (k -> Bool) ->  Dict k v -> Maybe (k, v)
findPairWith p = L.find (p . fst)

find :: Eq k => k -> Dict k v -> Maybe v
find = findWith . (==)

findWith :: (k -> Bool) -> Dict k v -> Maybe v
findWith = (fmap snd .) . findPairWith

partPair :: Eq k => k -> Dict k v -> ([(k, v)], [(k, v)])
partPair = partPairWith . (==)

partPairWith :: (k -> Bool) -> Dict k v -> ([(k, v)], [(k, v)])
partPairWith p = L.partition (p . fst)

part :: Eq k => k -> Dict k v -> ([v], [v])
part = partWith . (==)

partWith :: (k -> Bool) -> Dict k v -> ([v], [v])
partWith = (onPair (dup $ map snd) .) . partPairWith
