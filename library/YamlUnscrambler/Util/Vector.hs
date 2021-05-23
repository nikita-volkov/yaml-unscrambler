module YamlUnscrambler.Util.Vector
where

import YamlUnscrambler.Prelude as Prelude hiding (lookup)
import Data.Vector


lookupFirst :: [Int] -> Vector a -> Maybe (Int, a)
lookupFirst keys vector =
  getFirst (Prelude.foldMap (\ k -> First (fmap (k,) (vector !? k))) keys)
