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
  ByKeyMapping CaseSensitive ByKey

data Sequence =
  MonomorphicSequence Value
    |
  ByOrderSequence ByOrder
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

data ByKey =
  AnyByKey
    |
  NoByKey
    |
  EitherByKey ByKey ByKey
    |
  BothByKey ByKey ByKey
    |
  LookupByKey [Text] {-^ Keys to lookup. -} Value

data ByOrder =
  AnyByOrder
    |
  NoByOrder
    |
  EitherByOrder ByOrder ByOrder
    |
  BothByOrder ByOrder ByOrder
    |
  FetchByOrder Value

data ByIndex =
  AnyByIndex
    |
  NoByIndex
    |
  EitherByIndex ByIndex ByIndex
    |
  BothByIndex ByIndex ByIndex
    |
  LookupByIndex Int Value


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
