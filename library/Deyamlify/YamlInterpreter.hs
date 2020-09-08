module Deyamlify.YamlInterpreter
where

import Deyamlify.Prelude hiding (fail)
import Deyamlify.Model
import qualified Data.Yaml.Parser as Yaml
import qualified Text.Libyaml as Libyaml
import qualified Deyamlify.Util.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AsciiAtto
import qualified Data.Attoparsec.Text as TextAtto
import qualified Attoparsec.Time.ByteString as AsciiAtto
import qualified Data.UUID as UUID
import qualified Deyamlify.AsciiAtto as AsciiAtto
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Deyamlify.Util.HashMap as HashMap


data Err =
  Err
    [Text]
    {-^ Path. -}
    Text
    {-^ Message. -}

type Eff =
  ExceptT Err ((->) Env)

data Env =
  Env
    (Map String Yaml.YamlValue)
    {-^ Anchor map from the RawDoc. -}
    [Text]
    {-^ Path. -}

parseValue :: Yaml.YamlValue -> ValueParser a -> Eff a
parseValue node parser@(ValueParser scalarParsers mappingParserMb sequenceParserMb) =
  case node of
    Yaml.Scalar bytes tag style anchor ->
      case scalarParsers of
        [] ->
          fail "Unexpected scalar node"
        _ ->
          error "TODO"
    Yaml.Mapping mappings _ ->
      case mappingParserMb of
        Just p ->
          parseMapping mappings p
        Nothing ->
          fail "Unexpected mapping node"
    Yaml.Alias anchorName ->
      do
        node <- resolveAnchor anchorName
        parseValue node parser

parseMapping :: [(Text, Yaml.YamlValue)] -> MappingParser a -> Eff a
parseMapping input =
  \ case
    FoldingMappingParser keyParser valueParser (Fold foldStep foldInit foldExtract) ->
      foldM step foldInit input &
      fmap foldExtract
      where
        step !state (key, val) =
          do
            parsedKey <- atKey "[keys]" (atKey key (parseString key keyParser))
            parsedValue <- atKey key (parseValue val valueParser)
            return (foldStep state (parsedKey, parsedValue))
    KeyDeterminedFoldingMappingParser keyParser (Fold foldStep foldInit foldExtract) ->
      foldM step foldInit input &
      fmap foldExtract
      where
        step !state (key, val) =
          do
            valueParser <- atKey "[keys]" (atKey key (parseString key keyParser))
            assoc <- atKey key (parseValue val valueParser)
            return (foldStep state assoc)
    QueryingMappingParser caseSensitive fieldsParser ->
      parseFields input caseSensitive fieldsParser

parseFields :: [(Text, Yaml.YamlValue)] -> CaseSensitive -> FieldsParser a -> Eff a
parseFields input (CaseSensitive caseSensitive) =
  run
  where
    !map =
      if caseSensitive
        then
          HashMap.fromList input
        else
          HashMap.fromList (fmap (first Text.toLower) input)
    lookup =
      if caseSensitive
        then \ k -> HashMap.lookup k map
        else \ k -> HashMap.lookup (Text.toLower k) map
    lookupFirst =
      if caseSensitive
        then \ k -> HashMap.lookupFirst k map
        else \ k -> HashMap.lookupFirst (fmap Text.toLower k) map
    run :: FieldsParser a -> Eff a
    run =
      \ case
        PureFieldsParser a ->
          return a
        ApFieldsParser l r ->
          ($) <$> run l <*> run r
        SelectFieldsParser l r ->
          run l >>= \ case
            Left b ->
              run r & fmap ($ b)
            Right a ->
              return a
        EmptyFieldsParser ->
          fail ""
        AltFieldsParser l r ->
          ExceptT $ do
            lr <- runExceptT (run l)
            case lr of
              Left _ ->
                runExceptT (run r)
              Right lrr ->
                return (Right lrr)
        FieldFieldsParser key valueParser ->
          case lookup key of
            Just val ->
              atKey key (parseValue val valueParser)
            Nothing ->
              fail ("Key not found: " <> showAsText key)
        OneOfFieldsFieldsParser keys valueParser ->
          case lookupFirst keys of
            Just (key, val) ->
              atKey key (parseValue val valueParser)
            Nothing ->
              fail ("None of the keys found: " <> showAsText keys)

parseScalar :: ByteString -> Libyaml.Tag -> Libyaml.Style -> Libyaml.Anchor -> ScalarParser a -> Eff a
parseScalar bytes tag style anchor parser =
  case parser of
    StringScalarParser stringParser ->
      do
        text <- decodeUtf8 bytes
        parseString text stringParser
    NullScalarParser result ->
      if
        ByteString.null bytes ||
        bytes == "~" ||
        ByteString.saysNullInCiAscii bytes
        then
          return result
        else
          fail "Not null"
    BoolScalarParser cont ->
      if ByteString.length bytes <= 5
        then let
          lowerCased =
            ByteString.lowercaseInAscii bytes
          in if elem lowerCased ["y", "yes", "on", "true", "t", "1"]
            then
              return (cont True)
            else if elem lowerCased ["n", "no", "off", "false", "f", "0"]
              then
                return (cont False)
              else
                fail "Not a boolean"
        else
          fail "Not a boolean"
    ScientificScalarParser cont ->
      attoparseAscii bytes AsciiAtto.scientific &
      fmap cont
    DoubleScalarParser cont ->
      attoparseAscii bytes AsciiAtto.double &
      fmap cont
    RationalScalarParser (MaxInputSize maxInputSize) cont ->
      if ByteString.length bytes <= maxInputSize
        then
          attoparseAscii bytes AsciiAtto.rational &
          fmap cont
        else
          fail ("Input is larger then the expected maximum of " <> showAsText maxInputSize <> " bytes long")
    IntScalarParser signed numeralSystem cont ->
      attoparseAscii bytes (AsciiAtto.integralScalar signed numeralSystem) &
      fmap cont
    IntegerScalarParser (MaxInputSize maxInputSize) signed numeralSystem cont ->
      if ByteString.length bytes <= maxInputSize
        then
          attoparseAscii bytes (AsciiAtto.integralScalar signed numeralSystem) &
          fmap cont
        else
          fail ("Input is larger then the expected maximum of " <> showAsText maxInputSize <> " bytes long")
    UTCTimeScalarParser cont ->
      attoparseAscii bytes AsciiAtto.utcTimeInISO8601 &
      fmap cont
    DayScalarParser cont ->
      attoparseAscii bytes AsciiAtto.dayInISO8601 &
      fmap cont
    UUIDScalarParser cont ->
      case UUID.fromASCIIBytes bytes of
        Just uuid ->
          return (cont uuid)
        Nothing ->
          fail "Invalid UUID"
    BinaryScalarParser rectifier ->
      case tag of
        Libyaml.UriTag "tag:yaml.org,2002:binary" ->
          let
            bytesWithoutNewlines =
              ByteString.filter (/= 10) bytes
            in case Base64.decodeBase64 bytesWithoutNewlines of
              Right res ->
                case rectifier res of
                  Right res ->
                    return res
                  Left err ->
                    fail err
              Left err ->
                fail err
        _ ->
          fail "Not tagged as binary"

parseString :: Text -> StringParser a -> Eff a
parseString input =
  \ case
    TotalStringParser cont ->
      return (cont input)
    MappingStringParser caseSensitive map ->
      error "TODO"
    FormattedStringParser format rectifier ->
      error "TODO"

decodeUtf8 :: ByteString -> Eff Text
decodeUtf8 =
  error "TODO"

attoparseAscii :: ByteString -> AsciiAtto.Parser a -> Eff a
attoparseAscii =
  error "TODO"

resolveAnchor :: Libyaml.AnchorName -> Eff Yaml.YamlValue
resolveAnchor anchorName =
  do
    Env map _ <- ask
    case Map.lookup anchorName map of
      Just value ->
        return value
      Nothing ->
        fail ("No value found for anchor: " <> fromString anchorName)

{-|
Execute an effect adding a segment to its context path.
-}
atIndex :: Int -> Eff a -> Eff a
atIndex =
  atKey . showAsText

{-|
Execute an effect adding a segment to its context path.
-}
atKey :: Text -> Eff a -> Eff a
atKey segment =
  local (\ (Env map path) -> Env map (segment : path))

{-|
Raise an error message with current context path.
-}
fail :: Text -> Eff a
fail message =
  do
    Env _ path <- ask
    throwError (Err path message)
