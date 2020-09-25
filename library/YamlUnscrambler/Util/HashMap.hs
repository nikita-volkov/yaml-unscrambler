module YamlUnscrambler.Util.HashMap
where

import YamlUnscrambler.Prelude hiding (lookup)
import Data.HashMap.Strict


lookupFirst :: (Hashable k, Eq k) => [k] -> HashMap k v -> Maybe (k, v)
lookupFirst keys map =
  getFirst (foldMap (\ k -> First (fmap (k,) (lookup k map))) keys)
