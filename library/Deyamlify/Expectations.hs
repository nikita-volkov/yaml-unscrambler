module Deyamlify.Expectations
where

import Deyamlify.Prelude hiding (String)


-- *
-------------------------

{-|
Specification of the maximum allowed length for the input.
A safety measure to ensure that the parser doesn't exhaust memory
when parsing to unlimited datatypes.
-}
newtype MaxInputSize =
  MaxInputSize Int

newtype Signed =
  Signed Bool

data NumeralSystem =
  DecimalNumeralSystem
    |
  HexadecimalNumeralSystem

newtype CaseSensitive =
  CaseSensitive Bool

newtype Required =
  Required Bool


-- *
-------------------------

data Value =
  Value
    [Scalar]
    (Maybe Mapping)
    (Maybe Sequence)

data Scalar =
  StringScalar String
    |
  NullScalar
    |
  BoolScalar
    |
  ScientificScalar
    |
  DoubleScalar
    |
  RationalScalar MaxInputSize
    |
  IntScalar Signed NumeralSystem
    |
  IntegerScalar Signed NumeralSystem
    |
  Iso8601TimestampScalar
    |
  Iso8601DayScalar
    |
  Iso8601TimeScalar
    |
  UuidScalar
    |
  Base64BinaryScalar

data Mapping =
  UniformMapping String Value
    |
  VaryingMapping CaseSensitive Fields

data Sequence =
  MonomorphicSequence Value
    |
  PolymorphicSequence Elements

-- *
-------------------------

data String =
  {-| Any string as it is. -}
  AnyString
    |
  {-| One of options. Suitable for enumerations. -}
  OneOfString CaseSensitive [Text] {-^ Options. -}
    |
  {-| Must conform to a textually described format. -}
  FormattedString Text {-^ Description of the format. -}

data Fields =
  AnyFields
    |
  NoFields
    |
  EitherFields Fields Fields
    |
  BothFields Fields Fields
    |
  QueryFields [Text] {-^ Keys to lookup. -} Value

data Elements =
  AnyElements
    |
  NoElements
    |
  EitherElements Elements Elements
    |
  BothElements Elements Elements
    |
  QueryElements Value
