module YamlUnscrambler.Util.ByteString
where

import YamlUnscrambler.Prelude hiding (map, length)
import Data.ByteString
import qualified YamlUnscrambler.Util.Word8 as Word8


lowercaseInAscii =
  map Word8.lowercaseInAscii

{-# NOINLINE lowercaseNullInAscii #-}
lowercaseNullInAscii :: ByteString
lowercaseNullInAscii =
  "null"

saysNullInCiAscii :: ByteString -> Bool
saysNullInCiAscii a =
  length a == 4 && lowercaseInAscii a == lowercaseNullInAscii
