module Types.Merge
    ( TL, TR, TN, ProdManyL(..), ProdManyR(..)  -- Types
    , Dict, newDict                             -- Dict
    , toTL, toTR, toTN                          -- Before Merge
    , fromTL, fromTR                            -- During Merge
    , fromTN                                    -- After  Merge
    , rebindOne, rebind, rebindWith             -- Rebind
    ) where

import Utils ((|>) , (\\) , (...) , fork, onFst, onSnd)
import Types.TypeDef (IVar, IMany, ConstT, TypeSig(..))
import qualified Dict as D (Dict, insert, find)

---- Types ----

newtype LIVar = LIV IVar deriving (Eq, Show)
newtype RIVar = RIV IVar deriving (Eq, Show)

newtype LIMany = LIM IMany deriving (Eq, Show)
newtype RIMany = RIM IMany deriving (Eq, Show)

data TL
    = Lconst ConstT
    | Lfunc [TL] [TL]
    | Lvar LIVar
    | Lmany LIMany
    deriving (Show)

data TR
    = Rconst ConstT
    | Rfunc [TR] [TR]
    | Rvar RIVar
    | Rmany RIMany
    deriving (Show)

newtype TN = N TypeSig deriving (Show)

data ProdManyL
    = PMLnil
    | PMLone TN
    | PMLtwo TN TL
    deriving (Show)

data ProdManyR
    = PMRnil
    | PMRone TN
    | PMRtwo TN TR
    deriving (Show)

---- Gen ----

type Gen = (,) Int Int

newGen :: Gen
newGen = (,) 0 0

nextVar :: Gen -> (Gen, IVar)
nextVar = fork (,) (onFst (+1)) fst

nextMany :: Gen -> (Gen, IMany)
nextMany = fork (,) (onSnd (+1)) $ flip (,) 0 . snd

---- Dict ----

type DictL = (D.Dict LIVar TN, D.Dict LIMany ProdManyL)
type DictR = (D.Dict RIVar TN, D.Dict RIMany ProdManyR)
type Dict  = ((,) DictL DictR, Gen)

newDict :: Dict
newDict = (,) (([],[]), ([],[])) newGen

---- Dict Helpers ----

insertLV :: LIVar -> TN -> Dict -> Dict
insertLV = onFst ... onFst ... onFst ... D.insert

insertRV :: RIVar -> TN -> Dict -> Dict
insertRV = onFst ... onSnd ... onFst ... D.insert

insertLM :: LIMany -> ProdManyL -> Dict -> Dict
insertLM = onFst ... onFst ... onSnd ... D.insert

insertRM :: RIMany -> ProdManyR -> Dict -> Dict
insertRM = onFst ... onSnd ... onSnd ... D.insert

findLV :: LIVar -> Dict -> Maybe TN
findLV k = D.find k . fst . fst . fst

findRV :: RIVar -> Dict -> Maybe TN
findRV k = D.find k . fst . snd . fst

findLM :: LIMany -> Dict -> Maybe ProdManyL
findLM k = D.find k . snd . fst . fst

findRM :: RIMany -> Dict -> Maybe ProdManyR
findRM k = D.find k . snd . snd . fst

newLV :: LIVar -> Dict -> (Dict, TN)
newLV n (d, g) = let
        (g1, nt) = nextVar g
        t  = toTN $ Tvar nt
        d1 = insertLV n t (d, g1)
    in (d1, t)

newRV :: RIVar -> Dict -> (Dict, TN)
newRV n (d, g) = let
        (g1, nt) = nextVar g
        t  = toTN $ Tvar nt
        d1 = insertRV n t (d, g1)
    in (d1, t)

newLM :: LIMany -> Dict -> (Dict, TN)
newLM n (d, g) = let
        (g1, nt) = nextMany g
        t  = toTN $ Tmany nt
        d1 = insertLM n (PMLone t) (d, g1)
    in (d1, t)

newRM :: RIMany -> Dict -> (Dict, TN)
newRM n (d, g) = let
        (g1, nt) = nextMany g
        t  = toTN $ Tmany nt
        d1 = insertRM n (PMRone t) (d, g1)
    in (d1, t)

---- Before Merge ----

toTL :: TypeSig -> TL
toTL (Tconst c ) = Lconst c
toTL (Tfunc i o) = Lfunc (fmap toTL i) (fmap toTL o)
toTL (Tvar   n ) = Lvar (LIV n)
toTL (Tmany  n ) = Lmany (LIM n)

toTR :: TypeSig -> TR
toTR (Tconst c ) = Rconst c
toTR (Tfunc i o) = Rfunc (fmap toTR i) (fmap toTR o)
toTR (Tvar   n ) = Rvar (RIV n)
toTR (Tmany  n ) = Rmany (RIM n)

toTN :: TypeSig -> TN
toTN = N

---- Rebinding (During Merge) ----

fromTL :: TL -> Dict -> (Dict, Either ProdManyL TN)
fromTL (Lconst c ) d = (,) d $ Right $ toTN $ Tconst c
fromTL (Lfunc i o) d = let
    f d' ts x = case fromTL x d' of
            (d1', Right t) -> (d1', ts++[fromTN t])
            (d1', Left pm) -> case pm of
                PMLnil      -> (d1',  ts)
                PMLone t    -> (d1',  ts++[fromTN t])
                PMLtwo t ls -> f d1' (ts++[fromTN t]) ls
    fold = foldl $ uncurry  f
    (d1, ni) = fold (d , []) i
    (d2, no) = fold (d1, []) o
    in (,) d2 $ Right $ toTN $ Tfunc ni no
fromTL (Lvar  n  ) d = let
    (d1, nt) = newLV n d
    in case findLV n d of
        Just  t -> (d , Right  t)
        Nothing -> (d1, Right nt)
fromTL (Lmany n  ) d = let
    (d1, nt) = newLM n d
    in
    case findLM n d of
        Just pml -> (d , Left pml)
        Nothing  -> (d1, Right nt)

fromTR :: TR -> Dict -> (Dict, Either ProdManyR TN)
fromTR (Rconst c ) d = (,) d $ Right $ toTN $ Tconst c
fromTR (Rfunc i o) d = let
    f d' ts x = case fromTR x d' of
            (d1', Right t) -> (d1', ts++[fromTN t])
            (d1', Left pm) -> case pm of
                PMRnil      -> (d1',  ts)
                PMRone t    -> (d1',  ts++[fromTN t])
                PMRtwo t rs -> f d1' (ts++[fromTN t]) rs
    fold = foldl $ uncurry  f
    (d1, ni) = fold (d , []) i
    (d2, no) = fold (d1, []) o
    in (,) d2 $ Right $ toTN $ Tfunc ni no
fromTR (Rvar  n  ) d = let
    (d1, nt) = newRV n d
    in case findRV n d of
        Just  t -> (d , Right  t)
        Nothing -> (d1, Right nt)
fromTR (Rmany n  ) d = let
    (d1, nt) = newRM n d
    in
    case findRM n d of
        Just pmr -> (d , Left pmr)
        Nothing  -> (d1, Right nt)

---- Rebinding (After Merge) ----

fromTN :: TN -> TypeSig
fromTN (N t) = t

---- Utils ----

rebindOne :: TypeSig -> (Dict, TypeSig)
rebindOne = (:[]) \\ rebind \\ onSnd head

rebind :: [TypeSig] -> (Dict, [TypeSig])
rebind = rebindWith newDict

rebindWith :: Dict -> [TypeSig] -> (Dict, [TypeSig])
rebindWith d ts = do_rebind [] d $ fmap toTL ts

do_rebind :: [TN] -> Dict -> [TL] -> (Dict, [TypeSig])
do_rebind ts d    []  = (d, ts |> fmap fromTN |> reverse)
do_rebind ts d (y:ys) = case fromTL y d of
    (d1, Right        t   ) -> do_rebind (t:ts) d1    ys
    (d1, Left  PMLnil     ) -> do_rebind    ts  d1    ys
    (d1, Left (PMLone t  )) -> do_rebind (t:ts) d1    ys
    (d1, Left (PMLtwo t l)) -> do_rebind (t:ts) d1 (l:ys)
