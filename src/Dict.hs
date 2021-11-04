module Dict
    ( Dict
    , insert
    , findPair, find
    , partPair, part
    ) where

import Utils (onPair, dup)
import qualified Data.List as L (find, partition)

type Dict k v = [(k, v)]

insert :: k -> v -> Dict k v -> Dict k v
insert k v d = (k, v):d

findPair :: (k -> Bool) ->  Dict k v ->Maybe (k, v)
findPair p = L.find (p . fst)

find :: (k -> Bool) -> Dict k v -> Maybe v
find = (fmap snd .) . findPair

partPair :: (k -> Bool) -> Dict k v -> ([(k, v)], [(k, v)])
partPair p = L.partition (p . fst)

part :: (k -> Bool) -> Dict k v -> ([v], [v])
part = (onPair (dup $ map snd) .) . partPair
