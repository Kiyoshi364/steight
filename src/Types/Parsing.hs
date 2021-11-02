module Types.Parsing (typeP) where

import Parser
import Control.Applicative ((<|>), many)
import Types (TypeSig(..), ConstT(..))

typeP :: Parser TypeSig
typeP = tconstP <|> tfuncP <|> tvarP

tconstP :: Parser TypeSig
tconstP = strP "I64" *> whiteP *> pure (Tconst I64)

tfuncP :: Parser TypeSig
tfuncP = fmap Tfunc
    (charP '[' *> whiteP *> typesP <* strP "--")
    <*> (whiteP *> typesP <* charP ']' <* whiteP)
  where
    typesP :: Parser [TypeSig]
    typesP = many $ typeP <* whiteP

tvarP :: Parser TypeSig
tvarP = fmap Tvar (strP "'" *> numP <* whiteP)
