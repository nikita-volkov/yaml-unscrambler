module YamlUnscrambler.Expectations
where

import YamlUnscrambler.Prelude hiding (String)
import YamlUnscrambler.Model
import qualified YamlUnscrambler.Util.Maybe as Maybe


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
  BoundedIntegerScalar Signed NumeralSystem
    |
  UnboundIntegerScalar MaxInputSize Signed NumeralSystem
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
  ByOrderMapping ByOrder
    |
  ByKeyMapping CaseSensitive (ByKey Text)

data Sequence =
  MonomorphicSequence Value
    |
  ByOrderSequence ByOrder
    |
  ByKeySequence (ByKey Int)

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

data ByKey key =
  AnyByKey
    |
  NoByKey
    |
  EitherByKey (ByKey key) (ByKey key)
    |
  BothByKey (ByKey key) (ByKey key)
    |
  LookupByKey [key] {-^ Keys to lookup. -} Value

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
