module Deyamlify.Util.List
where

import Deyamlify.Prelude hiding (lookup)


firstNonEmpty :: [a] -> [a] -> [a]
firstNonEmpty a b =
  case a of
    [] -> b
    _ -> a
