module Deyamlify.Model
where

import Deyamlify.Prelude
import qualified Deyamlify.Util.Maybe as Maybe


-- * Types
-------------------------

data ValueParser a =
  ValueParser
    [ScalarParser a]
    (Maybe (MappingParser a))
    (Maybe (SequenceParser a))

-- **
-------------------------

data ScalarParser a =
  StringScalarParser (StringParser a)
    |
  NullScalarParser ~a
    |
  BoolScalarParser (Bool -> a)
    |
  ScientificScalarParser (Scientific -> a)
    |
  DoubleScalarParser (Double -> a)
    |
  RationalScalarParser MaxInputSize (Rational -> a)
    |
  IntScalarParser Signed NumeralSystem (Int -> a)
    |
  IntegerScalarParser MaxInputSize Signed NumeralSystem (Integer -> a)
    |
  UTCTimeScalarParser (UTCTime -> a)
    |
  DayScalarParser (Day -> a)
    |
  UUIDScalarParser (UUID -> a)
    |
  BinaryScalarParser (ByteString -> Either Text a)

data MappingParser a =
  forall key val. FoldingMappingParser (StringParser key) (ValueParser val) (Fold (key, val) a)
    |
  {-|
  The parser of values is determined by the result of parsing the key.
  -}
  forall assoc. KeyDeterminedFoldingMappingParser (StringParser (ValueParser assoc)) (Fold assoc a)
    |
  QueryingMappingParser CaseSensitive (FieldsParser a)

data SequenceParser a =
  forall val. HomogenousSequenceParser (ValueParser val) (Fold val a)
    |
  HeterogenousSequenceParser (ElementsParser a)

-- **
-------------------------

data StringParser a =
  TotalStringParser (Text -> a)
    |
  {-|
  Especially well-suited for enumerated values.
  -}
  MappingStringParser CaseSensitive [(Text, a)]
    |
  FormattedStringParser
    Text
    {-^ Description of the expected format. -}
    (Text -> Either Text a)
    {-^ Parsing function. -}

{-|
Heterogenous mappings.
-}
data FieldsParser a =
  PureFieldsParser a
    |
  forall b. ApFieldsParser (FieldsParser (b -> a)) (FieldsParser b)
    |
  forall b. SelectFieldsParser (FieldsParser (Either b a)) (FieldsParser (b -> a))
    |
  EmptyFieldsParser
    |
  AltFieldsParser (FieldsParser a) (FieldsParser a)
    |
  FieldFieldsParser Text (ValueParser a)
    |
  OneOfFieldsFieldsParser [Text] (ValueParser a)

{-|
Heterogenous sequence.
-}
data ElementsParser a =
  PureElementsParser a
    |
  forall b. ApElementsParser (ElementsParser (b -> a)) (ElementsParser b)
    |
  AltElementsParser (ElementsParser a) (ElementsParser a)
    |
  forall b. SelectElementsParser (ElementsParser (Either b a)) (ElementsParser (b -> a))
    |
  ElementElementsParser (ValueParser a)

-- **
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
  DecimalNumeralSystem |
  HexadecimalNumeralSystem

newtype CaseSensitive =
  CaseSensitive Bool


-- * Instances
-------------------------

deriving instance Functor ValueParser
deriving instance Functor ScalarParser
deriving instance Functor MappingParser
deriving instance Functor SequenceParser
deriving instance Functor StringParser
deriving instance Functor FieldsParser
deriving instance Functor ElementsParser

instance Applicative ScalarParser where
  pure =
    error "TODO"
  (<*>) =
    error "TODO"

instance Applicative MappingParser where
  pure =
    error "TODO"
  (<*>) =
    error "TODO"

instance Applicative SequenceParser where
  pure =
    error "TODO"
  (<*>) =
    error "TODO"

instance Semigroup (ValueParser a) where
  (<>) (ValueParser lScalar lMapping lSequence) (ValueParser rScalar rMapping rSequence) =
    ValueParser scalar mapping sequence
    where
      scalar =
        lScalar <> rScalar
      mapping =
        Maybe.firstNonEmpty lMapping rMapping
      sequence =
        Maybe.firstNonEmpty lSequence rSequence
