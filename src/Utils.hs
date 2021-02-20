-- |
-- Module      : Utils
-- Description : Utility functions
-- Copyright   : (c) Me, 2020
-- License     : GPL-3
-- Maintainer  : Pierre Penninckx (ibizapeanut@gmail.com)
-- Stability   : experimental
-- Portability : POSIX
--
-- Provides some various utility functions.
module Utils
  ( splitStringOnLastChar,
    lastMaybe,
  )
where

import qualified Data.List as List

-- | Split a string on the last occurrence of the given character.
splitStringOnLastChar :: Char -> String -> Maybe (String, String)
splitStringOnLastChar char string =
  (\index -> drop 1 <$> List.splitAt index string)
    <$> lastMaybe (List.elemIndices char string)

-- | Return the last element of the list.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe [x] = Just x
lastMaybe (_ : xs) = lastMaybe xs
