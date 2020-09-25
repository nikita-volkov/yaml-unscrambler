module YamlUnscrambler.Util.Vector
where

import YamlUnscrambler.Prelude hiding (lookup)
import Data.Vector


lookupFirst :: [Int] -> Vector a -> Maybe (Int, a)
lookupFirst keys vector =
  getFirst (foldMap (\ k -> First (fmap (k,) (vector !? k))) keys)
