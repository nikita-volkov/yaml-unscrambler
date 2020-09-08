module Deyamlify.Util.ByteString
where

import Deyamlify.Prelude hiding (map, length)
import Data.ByteString
import qualified Deyamlify.Util.Word8 as Word8


lowercaseInAscii =
  map Word8.lowercaseInAscii

{-# NOINLINE lowercaseNullInAscii #-}
lowercaseNullInAscii :: ByteString
lowercaseNullInAscii =
  "null"

saysNullInCiAscii :: ByteString -> Bool
saysNullInCiAscii a =
  length a == 4 && lowercaseInAscii a == lowercaseNullInAscii
