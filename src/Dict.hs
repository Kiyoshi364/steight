module Dict
    ( Dict
    , findPair, find
    , partPair, part
    ) where

import Utils (onPair, dup)
import qualified Data.List as L (find, partition)

type Dict k v = [(k, v)]

findPair :: (k -> Bool) ->  Dict k v ->Maybe (k, v)
findPair p = L.find (p . fst)

find :: (k -> Bool) -> Dict k v -> Maybe v
find = (fmap snd .) . findPair

partPair :: (k -> Bool) -> Dict k v -> ([(k, v)], [(k, v)])
partPair p = L.partition (p . fst)

part :: (k -> Bool) -> Dict k v -> ([v], [v])
part = (onPair (dup $ map snd) .) . partPair
