module NicePrint.AST
  ( astpp
  ) where

import IR.Token (Loc)
import IR.AST
    ( AST(..), ASTEntry(..), CaseDecl(..)
    , Inst(..), Instruction(..), TypeLit
    )
import qualified Dict as D (walk)
import NicePrint.PrettyPrint

astpp :: AST -> String
astpp = concatWith "\n\n"
    . D.walk (\ k v ->
        show k ++ ": " ++ astEntrypp v
    )
    . dict

astEntrypp :: ASTEntry -> String
astEntrypp (ASTBlock    l m_l_tl is) =
    ((++) $ show l ++ "\n") $ unlinesIndent
    $
        ( maybe [] with_type_at m_l_tl )
        ++ [ "with instructions:" ]
        ++ ( concat $ fmap (fmap indent . instpps) is )
astEntrypp (ASTTypeDecl l   l_tl cs) =
    ((++) $ show l ++ "\n") $ unlinesIndent
    $
        ( with_type_at l_tl )
        ++ [ "with cases:" ]
        ++ ( concat $ fmap (fmap indent . casesDeclpps) cs )

casesDeclpps :: (Loc, CaseDecl) -> [String]
casesDeclpps (loc, CaseDecl (l_s, s) (l_t, tl)) =
    ( (++) [ "case located at: " ++ show loc ] )
    $ fmap indent
    $
        [  "with name located at:" ++ show l_s ]
        ++ [ indent $ show s ]
        ++ [ "with type located at:" ++ show l_t]
        ++ [ indent $ show tl ]

instpps :: Inst -> [String]
instpps (Inst l i) =
    case instrpps i of
        []  -> error $ "NicePrint.AST.instpps: unrecheable: "
            ++ show (l, i)
        [s] -> [ show l ++ ": " ++ s ]
        ss  -> ( show l ++ ":" ) : fmap indent ss

instrpps :: Instruction -> [String]
instrpps (Push    x) = [ "Push " ++ show x ]
instrpps (Builtin b) = [ "Builtin " ++ show b ]
instrpps (PQuote is) =
    case is of
        []    -> [ "PQuote []" ]
        (_:_) -> [ "PQuote [" ]
            ++ ( concat $ fmap (fmap indent . instpps) is )
            ++ [ "]" ]
instrpps (PType typ) = [ "Type " ++ show typ ]
instrpps (Block m_name m_typ is) =
    [ "Block" ]
    ++ ( maybe ["unnamed"] (with_at show "name") m_name )
    ++ ( maybe ["untyped"] with_type_at m_typ )
    ++ ( concat $ fmap (fmap indent . instpps) is )
instrpps (TypeDecl l_name l_typ cs) =
    (++) [ "TypeDecl" ]
    $
        ( with_at show "name" l_name )
        ++ ( with_type_at l_typ )
        ++ [ "with cases:" ]
        ++ ( concat $ fmap (fmap indent . casesDeclpps) cs )
instrpps (Identifier ref) = [ "Identifier " ++ show ref ]

-- TODO: print TypeLit better
with_type_at :: (Loc, TypeLit) -> [String]
with_type_at = with_at show "type"

with_at :: (a -> String) -> String -> (Loc, a) -> [String]
with_at toStr thing (l_t, tl) =
        ( "with " ++ thing ++ " located at: " ++ show l_t )
        : ( indent $ toStr tl )
        : []
