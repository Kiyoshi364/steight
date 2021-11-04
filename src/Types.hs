module Types
    ( TypeSig(..)
    , ConstT(..)
    , match
    , compose
    ) where

import Utils (onSnd)
import qualified Dict as D (Dict)
import Dict (find)

type Dict = D.Dict (Either Int Int) TypeSig

data TypeSig
    = Tconst ConstT
    | Tfunc [TypeSig] [TypeSig]
    | Tvar Int
    deriving (Eq)

instance Show TypeSig where
    show (Tconst   t ) = show t
    show (Tfunc [] []) = "[]"
    show (Tfunc [] o ) = "[ " ++ revcat o ++ "]"
    show (Tfunc i  o ) = "[ " ++ revcat i ++ "-- " ++ revcat o ++ "]"
    show (Tvar     n ) = "'" ++ show n

data ConstT
    = I64
    deriving (Show, Eq)

revcat :: Show a => [a] -> String
revcat = foldr (\t s -> s ++ show t ++ " ") ""

rebind :: TypeSig -> TypeSig
rebind = snd . do_rebind []

do_rebind :: Dict -> TypeSig -> (Dict, TypeSig)
do_rebind d (Tconst  c) = (                  d, Tconst c)
do_rebind d (Tvar    n) = case find (== Right n) d of
    Just t  -> (                  d,      t)
    Nothing -> ((Right n, Tvar i):d, Tvar i)
  where i = length d
do_rebind d (Tfunc i o) = let
    (d'  , newi) = foldl (\ (d1, ts) t ->
        onSnd ((ts++) . (:[])) $ do_rebind d1 t) (d , []) i
    (newd, newo) = foldl (\ (d2, ts) t ->
        onSnd ((ts++) . (:[])) $ do_rebind d2 t) (d', []) o
    in (newd, Tfunc newi newo)

remapL :: Dict -> [TypeSig] -> [TypeSig]
remapL d = map $ \ t -> case t of
    Tvar n -> maybe t id $ find (== Left  n) d
    _      -> t

remapR :: Dict -> [TypeSig] -> [TypeSig]
remapR d = map $ \ t -> case t of
    Tvar n -> maybe t id $ find (== Right n) d
    _      -> t

match :: TypeSig -> TypeSig -> Maybe TypeSig
match y = fmap snd . do_match [] y

do_match :: Dict -> TypeSig -> TypeSig -> Maybe (Dict, TypeSig)
do_match d y x = case (y, x) of
    (          _, Tvar     nx) -> case find (== Right nx) d of
        Just ax -> do_match               d   y ax
        Nothing -> Just    ((Right nx, y):d,  y)
    (Tvar     ny,           _) -> case find (== Left  ny) d of
        Just ay -> do_match               d  ay  x
        Nothing -> Just    ((Left  ny, x):d,     x)
    (Tconst    _, Tconst    _) -> if y == x then Just $ (d, x) else Nothing
    (Tconst    _, Tfunc ix ox) -> if ix == [] && ox == [y]
        then Just $ (d, y) else Nothing
    (Tfunc iy oy, Tconst    _) -> if iy == [] && oy == [x]
        then Just $ (d, x) else Nothing
    (Tfunc iy oy, Tfunc ix ox) -> do
        (  d', newi) <- do_matches d iy ix
        (newd, newo) <- do_matches d' oy ox
        return (newd, Tfunc newi newo)

do_matches :: Dict -> [TypeSig] -> [TypeSig] -> Maybe (Dict, [TypeSig])
do_matches d  []     []    = Just (d, [])
do_matches d (y:ys) (x:xs) = do
    (d', t) <- do_match d y x
    fmap (onSnd (t:)) $ do_matches d' ys xs
do_matches _  _      _     = Nothing

try_compose :: [TypeSig] -> [TypeSig]
    -> Maybe (Dict, Either [TypeSig] [TypeSig])
try_compose = do_try_compose []

do_try_compose :: Dict -> [TypeSig] -> [TypeSig]
    -> Maybe (Dict, Either [TypeSig] [TypeSig])
do_try_compose d  []       xs  = Just $ (d, Left  $ remapR d xs)
do_try_compose d    ys     []  = Just $ (d, Right $ remapL d ys)
do_try_compose d (y:ys) (x:xs) = case do_match d y x of
    Just (d', _) -> do_try_compose d' ys xs
    Nothing      -> Nothing

compose :: TypeSig -> TypeSig -> Either String TypeSig
compose st@(Tfunc i1 o1) f@(Tfunc i2 o2) =
    case try_compose o1 i2 of
        Just (d, Left  ys) ->
            Right $ rebind $ Tfunc (remapL d i1 ++ ys) (remapR d o2      )
        Just (d, Right xs) ->
            Right $ rebind $ Tfunc (remapL d i1      ) (remapR d o2 ++ xs)
        Nothing            ->
            Left $ "Couldn't match types: stack `" ++ show st ++
            "`, function `" ++ show f ++ "`"
compose f1 (Tconst c) = compose f1 (Tfunc [] [Tconst c])
compose (Tconst c) f2 = compose (Tfunc [] [Tconst c]) f2
compose f1 f2 = error "Types.compose: Not handled inputs: compose (" ++
    show f1 ++ ") (" ++ show f2 ++ ")"
