module YamlUnscrambler.Util.List where

import YamlUnscrambler.Prelude hiding (lookup)

firstNonEmpty :: [a] -> [a] -> [a]
firstNonEmpty a b =
  case a of
    [] -> b
    _ -> a
