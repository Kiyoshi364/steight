module NicePrint.PrettyPrint
  ( indentStr, indent, indentTimes, unlinesIndent
  , concatWith
  , defaultAs
  ) where

indentStr :: String
indentStr = "    "

indent :: String -> String
indent = (++) indentStr

indentTimes :: Int -> String -> String
indentTimes n
  | n <= 0    = id
  | otherwise = indentTimes (n-1) . indent 

unlinesIndent :: [String] -> String
unlinesIndent = concatWith "\n" . fmap indent

concatWith :: String -> [String] -> String
concatWith str =
    maybe "" id
    . foldr (\ i m_s -> Just $ i ++ maybe "" (str ++) m_s) Nothing

defaultAs :: String -> String -> String
defaultAs def test = if test == "" then def else test
