-- |
module HumanReadable
  ( HumanReadable (..),
    printForHumans,
  )
where

class HumanReadable a where
  name :: a -> String
  body :: a -> [String]

printForHumans :: HumanReadable a => a -> String
printForHumans h = unlines $ name h : indent 2 (body h)

indent :: Int -> [String] -> [String]
indent n = map (spaces ++) where spaces = replicate n ' '
