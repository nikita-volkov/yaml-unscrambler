module Deyamlify.Util.Maybe
where

import Deyamlify.Prelude hiding (lookup)


firstNonEmpty :: Maybe a -> Maybe a -> Maybe a
firstNonEmpty a b =
  case a of
    Just a -> Just a
    _ -> b
