module Types
    ( ConstT(..)
    , TypeSig(..)
    , UserType(..)
    , UserCase(..)
    , Matched(..)
    , userType2destructorType
    , userType2constructors
    , match
    , compose
    ) where

import Types.TypeDef (IVar, IMany, ConstT(..), TypeSig(..))
import Types.UserType (UserType(..), UserCase(..))
import Types.UserTypeUtils
    ( userType2destructorType
    , userType2constructors
    )

import Utils (fork, onFst, onSnd)
import qualified Dict as D (Dict)
import Dict (find)

type DictVar  = D.Dict (Either IVar IVar) TypeSig
type DictMany = D.Dict (Either IMany IMany) ProdMany
type Dict     = (,) DictVar DictMany

type Gen = (Int, Int)

newGen :: Gen
newGen = (0, 0)

nextVar :: Gen -> (IVar, Gen)
nextVar = fork (,) fst $ onFst (+1)

nextMany :: Gen -> (IMany, Gen)
nextMany = fork (,) (flip (,) 0 . snd) $ onSnd (+1)

incM :: IMany -> IMany
incM = onSnd (+1)

tmInc :: IMany -> TypeSig
tmInc = Tmany . incM

data Matched
    = Mok    TypeSig
    | Mleft  TypeSig TypeSig
    | Mright TypeSig TypeSig
    deriving (Show)

mats2Typs :: [Matched] -> [TypeSig]
mats2Typs = flip foldr [] $ \ m xs -> case m of
    (Mok    t   ) -> [t]     ++ xs
    (Mleft  t ts) -> [t, ts] ++ xs
    (Mright t ts) -> [t, ts] ++ xs

data ProdMany
    = PMnil
    | PMone TypeSig
    | PMtwo TypeSig TypeSig
    deriving (Show)

pm2Typs :: ProdMany -> [TypeSig]
pm2Typs  PMnil       = []
pm2Typs (PMone t   ) = [t]
pm2Typs (PMtwo t ts) = [t, ts]

pm2 :: TypeSig -> IMany -> ProdMany
pm2 a n = PMtwo a $ tmInc n

rebind :: TypeSig -> TypeSig
rebind typ = (\ (_, _, m) -> case m of
        (Mok    t   ) -> t
        (Mleft  t ts) -> error $
            "Types.rebind.lambda: Not handled inputs: rebind (" ++
            show typ ++ ") found: Mleft  (" ++ show t ++ ") (" ++
            show ts ++ ")"
        (Mright t ts) -> error $
            "Types.rebind.lambda: Not handled inputs: rebind (" ++
            show typ ++ ") found: Mright (" ++ show t ++ ") (" ++
            show ts ++ ")"
    ) $ do_rebind newGen ([], []) typ

do_rebind :: Gen -> Dict -> TypeSig -> (Gen, Dict, Matched)
do_rebind g d         (Tconst  c) = (g,                  d, Mok $ Tconst c)
do_rebind g d@(dv,dm) (Tvar    n) = let (i, g') = nextVar g in
    case find (Right n) dv of
        Just t  -> (g ,                        d, Mok        t)
        Nothing -> (g',((Right n, Tvar i):dv,dm), Mok $ Tvar i)
do_rebind g d@(dv,dm) (Tmany   n) = let (j, g') = nextMany g in
    case find (Right n) dm of
        Just  PMnil       -> error $
            "Types.do_rebind: Not handled inputs: do_rebind (" ++
            show g ++ ") (" ++ show d ++ ") $ Just PMnil"
        Just (PMone t   ) -> (g, d, Mok    t   )
        Just (PMtwo t ts) -> (g, d, Mright t ts)
        Nothing     -> (g',(dv,(Right n, PMone $ Tmany j):dm), Mok $ Tmany j)
do_rebind g d         (Tfunc i o) = let
    join (g1, d1, ms) = (\ (g2,d2,m) -> (g2,d2,ms++[m])) . do_rebind g1 d1
    m2t (g1, d1, ms) = (g1, d1, mats2Typs ms)
    (g'  , d'  , newi) = m2t $ foldl join (g , d , []) i
    (newg, newd, newo) = m2t $ foldl join (g', d', []) o
    in (newg, newd, Mok $ Tfunc newi newo)

remapL :: Dict -> [TypeSig] -> [TypeSig]
remapL d@(dv,dm) = flip foldr [] $ \ t ts -> case t of
    Tvar  n   ->  (:ts) $ maybe t id $ find (Left  n) dv
    Tmany n   -> (++ts) $ maybe [t] (remapL d . pm2Typs) $ find (Left  n) dm
    Tfunc i o ->  (:ts) $ Tfunc (remapL d i) (remapL d o)
    _         -> (t:ts)

remapR :: Dict -> [TypeSig] -> [TypeSig]
remapR d@(dv,dm) = flip foldr [] $ \ t ts -> case t of
    Tvar  n   ->  (:ts) $ maybe t id $ find (Right n) dv
    Tmany n   -> (++ts) $ maybe [t] (remapR d . pm2Typs) $ find (Right n) dm
    Tfunc i o ->  (:ts) $ Tfunc (remapR d i) (remapR d o)
    _         -> (t:ts)

match :: TypeSig -> TypeSig -> Maybe Matched
match y = fmap snd . do_match ([], []) y

do_match :: Dict -> TypeSig -> TypeSig -> Maybe (Dict, Matched)
do_match d@(dv, dm) y x = case (y, x) of
    (          _, Tmany    nx) -> case find (Right nx) dm of
        Just  PMnil        -> Nothing
        Just (PMone ax   ) -> do_match d y ax
        Just (PMtwo ax bx) -> manyAfterR bx $ do_match d y ax
        Nothing -> case y of
            Tmany ny -> case find (Left  ny) dm of
                Just  PMnil        -> Nothing
                Just (PMone ay   ) -> do_match d ay x
                Just (PMtwo ay by) -> manyAfterR by $ do_match d ay x
                Nothing-> Just    ((dv,(Right nx, PMone y):dm), Mok y)
            _   -> Just    ((dv,(Right nx, pm2 y nx):dm), Mright y $ tmInc nx)
    (Tmany    ny,           _) -> case find (Left  ny) dm of
        Just  PMnil        -> Nothing
        Just (PMone ay   ) -> do_match d ay x
        Just (PMtwo ay by) -> manyAfterL by $ do_match d ay x
        Nothing -> Just    ((dv,(Left  ny, pm2 x ny):dm), Mleft  x $ tmInc ny)
    (          _, Tvar     nx) -> case find (Right nx) dv of
        Just ax -> do_match                   d     y ax
        Nothing -> Just    (((Right nx, y):dv,dm), Mok y)
    (Tvar     ny,           _) -> case find (Left  ny) dv of
        Just ay -> do_match                   d    ay  x
        Nothing -> Just    (((Left  ny, x):dv,dm), Mok x)
    (Tconst    _, Tconst    _) -> if y == x
        then Just $ (d, Mok x) else Nothing
    (Tconst    _, Tfunc ix ox) -> if ix == [] && ox == [y]
        then Just $ (d, Mok y) else Nothing
    (Tfunc iy oy, Tconst    _) -> if iy == [] && oy == [x]
        then Just $ (d, Mok x) else Nothing
    (Tfunc iy oy, Tfunc ix ox) -> do
        (  d', newi) <- do_matches d iy ix
        (newd, newo) <- do_matches d' oy ox
        return (newd, Mok $ Tfunc newi newo)
  where
    manyAfterR :: TypeSig -> Maybe (Dict, Matched) -> Maybe (Dict, Matched)
    manyAfterR _ Nothing             = Nothing
    manyAfterR j (Just (d1, Mok y1)) = Just (d1, Mright y1 j)
    manyAfterR j (Just (d1,      m)) = error $
        "Types.do_match.manyAfterR: Not handled inputs: manyAfterR (" ++
            show j ++ ") $ Just $ (" ++ show d1 ++ "), (" ++ show m ++ ")"

    manyAfterL :: TypeSig -> Maybe (Dict, Matched) -> Maybe (Dict, Matched)
    manyAfterL _ Nothing             = Nothing
    manyAfterL j (Just (d1, Mok x1)) = Just (d1, Mleft  x1 j)
    manyAfterL j (Just (d1,      m)) = error $
        "Types.do_match.manyAfterL: Not handled inputs: manyAfterL (" ++
            show j ++ ") $ Just $ (" ++ show d1 ++ "), (" ++ show m ++ ")"

do_matches :: Dict -> [TypeSig] -> [TypeSig] -> Maybe (Dict, [TypeSig])
do_matches  d       []     []    = Just (d, [])
do_matches (dv,dm) [y]     []    = case y of
    Tmany ny -> Just ((dv,(Left  ny, PMnil):dm), [])
    _        -> error "Tmany y"
do_matches (dv,dm)  []    [x]    = case x of
    Tmany nx -> Just ((dv,(Right nx, PMnil):dm), [])
    _        -> error "Tmany x"
do_matches  d      (y:ys) (x:xs) = do
    (d', m) <- do_match d y x
    let f t l1 l2 = fmap (onSnd (t:)) $ do_matches d' l1 l2
    case m of
            (Mok    t   ) -> f t     ys      xs
            (Mleft  t ts) -> f t (ts:ys)     xs
            (Mright t ts) -> f t     ys  (ts:xs)
do_matches _  _      _     = Nothing

try_compose :: [TypeSig] -> [TypeSig]
    -> Maybe (Dict, Either [TypeSig] [TypeSig])
try_compose = do_try_compose ([], [])

do_try_compose :: Dict -> [TypeSig] -> [TypeSig]
    -> Maybe (Dict, Either [TypeSig] [TypeSig])
do_try_compose d  []       xs  = Just $ (d, Left  $ remapR d xs)
do_try_compose d    ys     []  = Just $ (d, Right $ remapL d ys)
do_try_compose d (y:ys) (x:xs) = do
    (d', m) <- do_match d y x
    case m of
        (Mok    _   ) -> do_try_compose d'     ys      xs
        (Mleft  _ y1) -> do_try_compose d' (y1:ys)     xs
        (Mright _ x1) -> do_try_compose d'     ys  (x1:xs)

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
compose f1 f2 = error $ "Types.compose: Not handled inputs: compose (" ++
    show f1 ++ ") (" ++ show f2 ++ ")"
