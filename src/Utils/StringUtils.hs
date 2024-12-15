module Utils.StringUtils (toSlug) where

import Data.Char (toLower)

toSlug :: String -> String
toSlug str =
  let lowerStr = map toLower str
      replaceChar ' ' = '-'
      replaceChar c = c
   in map replaceChar lowerStr