module Deyamlify.AsciiAtto
where

import Deyamlify.Prelude
import Deyamlify.Model
import Data.Attoparsec.ByteString.Char8
import Attoparsec.Time.ByteString


integralScalar :: (Integral a, Bits a) => Signed -> NumeralSystem -> Parser a
integralScalar (Signed isSigned) numeralSystem =
  if isSigned
    then
      signed numeralParser
    else
      numeralParser
  where
    numeralParser =
      case numeralSystem of
        DecimalNumeralSystem ->
          decimal
        HexadecimalNumeralSystem ->
          hexadecimal
