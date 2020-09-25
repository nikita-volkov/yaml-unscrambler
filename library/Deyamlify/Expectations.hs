module Deyamlify.Expectations
where

import Deyamlify.Prelude hiding (String)
import Deyamlify.Model
import qualified Deyamlify.Util.Maybe as Maybe


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
  MonomorphicMapping String Value
    |
  PolymorphicMapping CaseSensitive Fields

data Sequence =
  MonomorphicSequence Value
    |
  ByPositionSequence ByPosition
    |
  ByIndexSequence ByIndex

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

data ByPosition =
  AnyByPosition
    |
  NoByPosition
    |
  EitherByPosition ByPosition ByPosition
    |
  BothByPosition ByPosition ByPosition
    |
  FetchByPosition Value

data ByIndex =
  AnyByIndex
    |
  NoByIndex
    |
  EitherByIndex ByIndex ByIndex
    |
  BothByIndex ByIndex ByIndex
    |
  IndexByIndex Int Value


-- *
-------------------------

instance Semigroup Value where
  (<>) (Value lScalars lMappings lSequences) (Value rScalars rMappings rSequences) =
    Value
      (lScalars <> rScalars)
      (Maybe.firstNonEmpty lMappings rMappings)
      (Maybe.firstNonEmpty lSequences rSequences)

instance Monoid Value where
  mempty =
    Value [] Nothing Nothing
