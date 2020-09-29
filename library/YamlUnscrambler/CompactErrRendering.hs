module YamlUnscrambler.CompactErrRendering
(
  renderErrAtPath,
)
where

import YamlUnscrambler.Prelude hiding (intercalate)
import YamlUnscrambler.Model
import Text.Builder
import qualified YamlUnscrambler.Err as Err
import qualified YamlUnscrambler.Expectations as Ex
import qualified Data.Text.Encoding as Text


renderErrAtPath :: Err.ErrAtPath -> Text
renderErrAtPath =
  run . errAtPath

path a =
  "/" <> intercalate "/" (fmap text a)

errAtPath (Err.ErrAtPath a b) =
  "Error at path " <> path a <> ". " <> reason b

reason =
  \ case
    Err.KeyErr a b c ->
      text c <> ". On input: " <> string (show b) <> ". " <>
      "Expecting: " <> stringExpectation a
    Err.NoneOfMappingKeysFoundErr a b c ->
      "None of keys found: " <> string (show c) <> ", " <>
      caseSensitive b
    Err.NoneOfSequenceKeysFoundErr a b ->
      "None of indices found: " <> string (show b)
    Err.ScalarErr a b c d e ->
      foldMap (\ a -> text a <> ". ") e <>
      "Expecting one of the following formats: " <>
      intercalate ", " (fmap scalarExpectation a) <>
      foldMap (\ a -> ". Got input: " <> text a) (Text.decodeUtf8' b)
    Err.UnexpectedScalarErr a ->
      "Unexpected scalar value"
    Err.UnexpectedMappingErr a ->
      "Unexpected mapping value"
    Err.UnexpectedSequenceErr a ->
      "Unexpected sequence value"
    Err.UnknownAnchorErr a ->
      "Unknown anchor: " <> text a
    Err.NotEnoughElementsErr a b ->
      "Not enough elements: " <> decimal b <> ". " <>
      "Expecting: " <> byOrderExpectation a

scalarExpectation =
  \ case
    Ex.StringScalar a ->
      stringExpectation a
    Ex.NullScalar ->
      "null"
    Ex.BoolScalar ->
      "boolean"
    Ex.ScientificScalar ->
      "scientific"
    Ex.DoubleScalar ->
      "double"
    Ex.RationalScalar a ->
      "rational of maximum length of " <> maxInputSize a <> " chars"
    Ex.BoundedIntegerScalar a b ->
      signed a <> " " <> numeralSystem b
    Ex.UnboundedIntegerScalar a b c ->
      signed b <> " " <> numeralSystem c <> " of maximum length of " <> maxInputSize a <> " chars"
    Ex.Iso8601TimestampScalar ->
      "timestamp in ISO-8601"
    Ex.Iso8601DayScalar ->
      "date in ISO-8601"
    Ex.Iso8601TimeScalar ->
      "time in ISO-8601"
    Ex.UuidScalar ->
      "UUID"
    Ex.Base64BinaryScalar ->
      "binary data in Base-64"

stringExpectation =
  \ case
    Ex.AnyString ->
      "any string"
    Ex.OneOfString a b ->
      "one of " <> string (show b) <> "(" <> caseSensitive a <> ")"
    Ex.FormattedString a ->
      text a

byOrderExpectation =
  decimal . count 0
  where
    count !a =
      \ case
        Ex.AnyByOrder ->
          a
        Ex.BothByOrder b c ->
          countBoth a b c
        Ex.FetchByOrder _ ->
          succ a
    countBoth a b c =
      case b of
        Ex.BothByOrder d e ->
          countBoth a d (Ex.BothByOrder e c)
        Ex.AnyByOrder ->
          count a c
        Ex.FetchByOrder _ ->
          count (succ a) c

caseSensitive (CaseSensitive a) =
  "case " <> bool "insensitive" "sensitive" a

signed (Signed a) =
  bool "unsigned" "signed" a

numeralSystem =
  \ case
    DecimalNumeralSystem ->
      "decimal"
    HexadecimalNumeralSystem ->
      "hexadecimal"

maxInputSize (MaxInputSize a) =
  decimal a
