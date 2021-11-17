module Types.Parsing (typeP) where

import Parser
import Control.Applicative ((<|>), many)
import Types (TypeSig(..), ConstT(..))

typeP :: Parser TypeSig
typeP = tconstP <|> tfuncP <|> tvarP <|> tmanyP

tconstP :: Parser TypeSig
tconstP = strP "I64" *> whiteP *> pure (Tconst I64)

tfuncP :: Parser TypeSig
tfuncP = fmap Tfunc
        (charP '[' *> whiteP *> typesP <* strP "--")
        <*> (whiteP *> typesP <* charP ']' <* whiteP)
    <|>
        fmap Tfunc
        (pure []) <*> (charP '[' *>
        whiteP *> typesP <* charP ']' <* whiteP)
  where
    typesP :: Parser [TypeSig]
    typesP = fmap reverse $ many $ typeP <* whiteP

tvarP :: Parser TypeSig
tvarP = fmap Tvar (strP "'" *> numP <* whiteP)

tmanyP :: Parser TypeSig
tmanyP = fmap (Tmany . flip (,) 0) (strP "!" *> numP <* whiteP)
