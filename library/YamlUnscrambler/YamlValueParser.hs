module YamlUnscrambler.YamlValueParser
where

import YamlUnscrambler.Prelude hiding (fail, liftEither)
import YamlUnscrambler.Model
import qualified Data.Yaml.Parser as Yaml
import qualified Text.Libyaml as Libyaml
import qualified YamlUnscrambler.Util.ByteString as ByteString
import qualified Data.ByteString as ByteString
import qualified Data.Attoparsec.ByteString.Char8 as AsciiAtto
import qualified Data.Attoparsec.Text as TextAtto
import qualified Attoparsec.Time.ByteString as AsciiAtto
import qualified Data.UUID as UUID
import qualified YamlUnscrambler.AsciiAtto as AsciiAtto
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified YamlUnscrambler.Util.HashMap as HashMap
import qualified YamlUnscrambler.Util.Text as Text
import qualified YamlUnscrambler.Util.Yaml as Yaml
import qualified Data.List as List


-- *
-------------------------

data Err =
  Err
    [Text]
    {-^ Path. -}
    Text
    {-^ Message. -}

type Eff =
  ExceptT [Err] ((->) Env)

data Env =
  Env
    (Map String Yaml.YamlValue)
    {-^ Anchor map from the RawDoc. -}
    [Text]
    {-^ Path. -}


runValueParser :: (Yaml.YamlValue -> Eff a) -> ByteString -> Either Text a
runValueParser eff input =
  do
    Yaml.RawDoc value map <- Yaml.parseByteStringToRawDoc input
    first errMapping (runExceptT (eff value) (Env map []))
  where
    errMapping =
      Text.intercalate "\n" . fmap renderErr
    renderErr (Err path message) =
      foldMap (\ x -> "/" <> x) (reverse path) <> ": " <>
      message


-- *
-------------------------

parseValue ::
  Maybe (ByteString -> Libyaml.Tag -> Libyaml.Style -> Eff a) ->
  Maybe ([(Text, Yaml.YamlValue)] -> Eff a) ->
  Maybe ([Yaml.YamlValue] -> Eff a) ->
  Yaml.YamlValue ->
  Eff a
parseValue scalarHandlers mappingHandlers sequenceHandlers =
  parse
  where
    parse =
      \ case
        Yaml.Scalar bytes tag style _ ->
          case scalarHandlers of
            Just scalarHandler ->
              scalarHandler bytes tag style
            Nothing ->
              fail "Unexpected scalar node"
        Yaml.Mapping input _ ->
          case mappingHandlers of
            Just mappingHandler ->
              mappingHandler input
            Nothing ->
              fail "Unexpected mapping node"
        Yaml.Sequence input _ ->
          case sequenceHandlers of
            Just sequenceHandler ->
              sequenceHandler input
            Nothing ->
              fail "Unexpected sequence node"
        Yaml.Alias anchorName ->
          do
            node <- resolveAnchor anchorName
            parse node

foldMappingIndividually :: (Text -> Eff key) -> (Yaml.YamlValue -> Eff val) -> Fold (key, val) a -> [(Text, Yaml.YamlValue)] -> Eff a
foldMappingIndividually parseKey parseVal fold input =
  foldMapping parse fold input
  where
    parse key val =
      (,) <$> parseKey key <*> parseVal val

foldMapping :: (Text -> Yaml.YamlValue -> Eff assoc) -> Fold assoc a -> [(Text, Yaml.YamlValue)] -> Eff a
foldMapping parse (Fold foldStep foldInit foldExtract) input =
  foldM step foldInit input &
  fmap foldExtract
  where
    step !state (key, val) =
      foldStep state <$> atKey key (parse key val)

foldSequence :: (Yaml.YamlValue -> Eff a) -> Fold a b -> [(Yaml.YamlValue)] -> Eff b
foldSequence parseElement (Fold foldStep foldInit foldExtract) input =
  foldM step (0, foldInit) input &
  fmap (foldExtract . snd)
  where
    step (!index, !state) element =
      (succ index,) . foldStep state <$> atIndex index (parseElement element)

parseScalarAsNull :: ByteString -> Libyaml.Tag -> Eff ()
parseScalarAsNull bytes tag =
  if
    tag == Libyaml.NullTag ||
    ByteString.null bytes ||
    bytes == "~" ||
    ByteString.saysNullInCiAscii bytes
    then
      return ()
    else
      fail "Not null"

parseScalarAsBool :: ByteString -> Eff Bool
parseScalarAsBool bytes =
  if ByteString.length bytes <= 5
    then let
      lowercased =
        ByteString.lowercaseInAscii bytes
      in if elem lowercased ["y", "yes", "on", "true", "t", "1"]
        then
          return True
        else if elem lowercased ["n", "no", "off", "false", "f", "0"]
          then
            return False
          else
            fail "Not a boolean"
    else
      fail "Not a boolean"

parseScalarAsString :: (Text -> Eff a) -> ByteString -> Eff a
parseScalarAsString cont bytes =
  decodeUtf8 bytes >>= cont

parseScalarAsScientific :: ByteString -> Eff Scientific
parseScalarAsScientific bytes =
  attoparseAscii AsciiAtto.scientific bytes

parseScalarAsDouble :: ByteString -> Eff Double
parseScalarAsDouble bytes =
  attoparseAscii AsciiAtto.double bytes

parseScalarAsRational :: MaxInputSize -> ByteString -> Eff Rational
parseScalarAsRational (MaxInputSize maxInputSize) bytes =
  if ByteString.length bytes <= maxInputSize
    then
      attoparseAscii AsciiAtto.rational bytes
    else
      fail ("Input is larger then the expected maximum of " <> showAsText maxInputSize <> " bytes long")

parseScalarAsBoundedInteger :: (Integral a, FiniteBits a) => Signed -> NumeralSystem -> ByteString -> Eff a
parseScalarAsBoundedInteger signed numeralSystem bytes =
  attoparseAscii (AsciiAtto.integralScalar signed numeralSystem) bytes

parseScalarAsUnboundInteger :: MaxInputSize -> Signed -> NumeralSystem -> ByteString -> Eff Integer
parseScalarAsUnboundInteger (MaxInputSize maxInputSize) signed numeralSystem bytes =
  if ByteString.length bytes <= maxInputSize
    then
      attoparseAscii (AsciiAtto.integralScalar signed numeralSystem) bytes
    else
      fail ("Input is larger then the expected maximum of " <> showAsText maxInputSize <> " bytes long")

parseScalarAsIsoTimestamp :: ByteString -> Eff UTCTime
parseScalarAsIsoTimestamp bytes =
  attoparseAscii AsciiAtto.utcTimeInISO8601 bytes

parseScalarAsIsoDate :: ByteString -> Eff Day
parseScalarAsIsoDate bytes =
  attoparseAscii AsciiAtto.dayInISO8601 bytes

parseScalarAsIsoTime :: ByteString -> Eff TimeOfDay
parseScalarAsIsoTime bytes =
  attoparseAscii AsciiAtto.timeOfDayInISO8601 bytes

parseScalarAsUuid :: ByteString -> Eff UUID
parseScalarAsUuid bytes =
  case UUID.fromASCIIBytes bytes of
    Just uuid ->
      return uuid
    Nothing ->
      fail "Invalid UUID"

parseScalarAsBase64Binary :: ByteString -> Eff ByteString
parseScalarAsBase64Binary bytes =
  let
    bytesWithoutNewlines =
      ByteString.filter (/= 10) bytes
    in case Base64.decodeBase64 bytesWithoutNewlines of
      Right res ->
        return res
      Left err ->
        fail err

parseScalarAsBase64BinaryCheckingTag :: ByteString -> Libyaml.Tag -> Eff ByteString
parseScalarAsBase64BinaryCheckingTag bytes tag =
  case tag of
    Libyaml.UriTag "tag:yaml.org,2002:binary" ->
      parseScalarAsBase64Binary bytes
    _ ->
      fail "Not tagged as binary"

mapString :: Bool -> [(Text, a)] -> Text -> Eff a
mapString caseSensitive mappingList =
  let
    expectedValuesText =
      fromString (show (fmap fst mappingList))
    mappingListLength =
      length mappingList
    !lookup =
      if mappingListLength > 512
        then if caseSensitive
          then let
            hashMap =
              HashMap.fromList mappingList
            in flip HashMap.lookup hashMap
          else let
            hashMap =
              HashMap.fromList (fmap (first Text.toLower) mappingList)
            in flip HashMap.lookup hashMap . Text.toLower
        else if caseSensitive
          then
            flip List.lookup mappingList
          else
            flip List.lookup (fmap (first Text.toLower) mappingList) . Text.toLower
    parse text =
      case lookup text of
        Just a -> return a
        _ -> fail ("Unexpected value: \"" <> text <> "\". Expecting one of: " <> expectedValuesText)
    in parse

decodeUtf8 :: ByteString -> Eff Text
decodeUtf8 =
  liftStringEither . first show . Text.decodeUtf8'

attoparseAscii :: AsciiAtto.Parser a -> ByteString -> Eff a
attoparseAscii parser =
  liftStringEither . AsciiAtto.parseOnly (parser <* AsciiAtto.endOfInput)

attoparseText :: TextAtto.Parser a -> Text -> Eff a
attoparseText parser =
  liftStringEither . TextAtto.parseOnly (parser <* TextAtto.endOfInput)

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

atShowableKey :: Show key => key -> Eff a -> Eff a
atShowableKey key =
  atKey (Text.deshowIfPossible (showAsText key))

{-|
Raise an error message with current context path.
-}
fail :: Text -> Eff a
fail message =
  do
    Env _ path <- ask
    throwError [Err path message]

liftEither :: Either Text a -> Eff a
liftEither =
  either fail return

liftStringEither :: Either String a -> Eff a
liftStringEither =
  either (fail . fromString) return