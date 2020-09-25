module Deyamlify.Dsl
where

import Deyamlify.Prelude hiding (String)
import Deyamlify.Model
import qualified Deyamlify.YamlValueParser as Parser
import qualified Data.Yaml.Parser as Yaml
import qualified Text.Libyaml as Libyaml
import qualified Deyamlify.Expectations as Ex
import qualified Control.Foldl as Fold
import qualified Data.HashMap.Strict as HashMap
import qualified Deyamlify.Util.HashMap as HashMap
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as TextAtto


-- * Execution
-------------------------

parseByteString :: Value a -> ByteString -> Either Text a
parseByteString =
  error "TODO"

{-|
Get a tree of expectations, which can then be converted into
documentation for people working with the YAML document or
into one of the spec formats (e.g., YAML Spec, JSON Spec).
-}
getExpectations :: Value a -> Ex.Value
getExpectations =
  error "TODO"


-- *
-------------------------

data Value a =
  Value {
    valueExpectation :: Ex.Value,
    valueParser :: Yaml.YamlValue -> Parser.Eff a
  }
  deriving (Functor)

value :: [Scalar a] -> Maybe (Mapping a) -> Maybe (Sequence a) -> Value a
value scalars mappings sequences =
  Value
    (Ex.Value
      (fmap scalarExpectation scalars)
      (fmap mappingExpectation mappings)
      (fmap sequenceExpectation sequences))
    parse
  where
    parse =
      \ case
        Yaml.Scalar bytes tag style _ ->
          asum (fmap parse scalars)
          where
            parse scalar =
              scalarParser scalar bytes tag style
        Yaml.Mapping input _ ->
          case mappings of
            Just mapping ->
              mappingParser mapping input
            Nothing ->
              Parser.fail "Unexpected mapping node"
        Yaml.Sequence input _ ->
          case sequences of
            Just sequence ->
              sequenceParser sequence input
            Nothing ->
              Parser.fail "Unexpected sequence node"
        Yaml.Alias anchorName ->
          do
            node <- Parser.resolveAnchor anchorName
            parse node


-- *
-------------------------

data Scalar a =
  Scalar {
    scalarExpectation :: Ex.Scalar,
    scalarParser :: ByteString -> Libyaml.Tag -> Libyaml.Style -> Parser.Eff a
  }
  deriving (Functor)

stringScalar :: String a -> Scalar a
stringScalar (String exp parse) =
  Scalar
    (Ex.StringScalar exp)
    (\ bytes tag style -> Parser.decodeUtf8 bytes >>= parse)

nullScalar :: Scalar ()
nullScalar =
  Scalar Ex.NullScalar (\ bytes tag _ -> Parser.parseScalarAsNull bytes tag)

boolScalar :: Scalar Bool
boolScalar =
  Scalar Ex.BoolScalar (\ bytes _ _ -> Parser.parseScalarAsBool bytes)

scientificScalar :: Scalar Scientific
scientificScalar =
  Scalar Ex.ScientificScalar (\ bytes _ _ -> Parser.parseScalarAsScientific bytes)

doubleScalar :: Scalar Double
doubleScalar =
  Scalar Ex.DoubleScalar (\ bytes _ _ -> Parser.parseScalarAsDouble bytes)

timestampScalar :: Scalar UTCTime
timestampScalar =
  Scalar Ex.Iso8601TimestampScalar (\ bytes _ _ -> Parser.parseScalarAsIsoTimestamp bytes)

dayScalar :: Scalar Day
dayScalar =
  Scalar Ex.Iso8601DayScalar (\ bytes _ _ -> Parser.parseScalarAsIsoDate bytes)

timeScalar :: Scalar TimeOfDay
timeScalar =
  Scalar Ex.Iso8601TimeScalar (\ bytes _ _ -> Parser.parseScalarAsIsoTime bytes)

uuidScalar :: Scalar UUID
uuidScalar =
  Scalar Ex.UuidScalar (\ bytes _ _ -> Parser.parseScalarAsUuid bytes)

binaryScalar :: Scalar ByteString
binaryScalar =
  Scalar Ex.Base64BinaryScalar (\ bytes _ _ -> Parser.parseScalarAsBase64Binary bytes)


-- *
-------------------------

data Mapping a =
  Mapping {
    mappingExpectation :: Ex.Mapping,
    mappingParser :: [(Text, Yaml.YamlValue)] -> Parser.Eff a
  }
  deriving (Functor)

assocVectorMapping :: String key -> Value val -> (key -> val -> assoc) -> Mapping (Vector assoc)
assocVectorMapping key val zip =
  Mapping
    (Ex.MonomorphicMapping (stringExpectation key) (valueExpectation val))
    (Parser.foldMapping parse Fold.vector)
  where
    parse k v =
      zip <$> stringParser key k <*> valueParser val v

fieldsMapping :: Bool -> Fields a -> Mapping a
fieldsMapping caseSensitive fields =
  Mapping expectation parse
  where
    expectation =
      Ex.PolymorphicMapping (CaseSensitive caseSensitive) (fieldsExpectation fields)
    parse input =
      if caseSensitive
        then let
          map =
            HashMap.fromList input
          lookup k =
            HashMap.lookup k map
          lookupFirst kl =
            HashMap.lookupFirst kl map
          in fieldsParser fields lookup lookupFirst
        else let
          map =
            HashMap.fromList (fmap (first Text.toLower) input)
          lookup k =
            HashMap.lookup (Text.toLower k) map
          lookupFirst kl =
            HashMap.lookupFirst (fmap Text.toLower kl) map
          in fieldsParser fields lookup lookupFirst


-- *
-------------------------

data Sequence a =
  Sequence {
    sequenceExpectation :: Ex.Sequence,
    sequenceParser :: [Yaml.YamlValue] -> Parser.Eff a
  }
  deriving (Functor)


-- *
-------------------------

data String a =
  String {
    stringExpectation :: Ex.String,
    stringParser :: Text -> Parser.Eff a
  }
  deriving (Functor)

textString :: String Text
textString =
  String Ex.AnyString return

mapperString :: Bool -> [(Text, a)] -> String a
mapperString caseSensitive assocList =
  String
    (Ex.OneOfString (CaseSensitive caseSensitive) (fmap fst assocList))
    (Parser.mapString caseSensitive assocList)

formattedString :: Text -> (Text -> Either Text a) -> String a
formattedString format matcher =
  String
    (Ex.FormattedString format)
    (Parser.liftEither . matcher)

attoparsedString :: Text -> TextAtto.Parser a -> String a
attoparsedString format parser =
  String
    (Ex.FormattedString format)
    (Parser.attoparseText parser)


-- *
-------------------------

data Fields a =
  Fields {
    fieldsExpectation :: Ex.Fields,
    fieldsParser :: (Text -> Maybe Yaml.YamlValue) -> ([Text] -> Maybe (Text, Yaml.YamlValue)) -> Parser.Eff a
  }
  deriving (Functor)

instance Applicative Fields where
  pure =
    Fields Ex.AnyFields . const . const . pure
  (<*>) (Fields le lp) (Fields re rp) =
    Fields
      (Ex.BothFields le re)
      (\ a b -> ($) <$> lp a b <*> rp a b)

instance Selective Fields where
  select (Fields le lp) (Fields re rp) =
    Fields
      (Ex.BothFields le re)
      (\ a b -> lp a b >>= either (\ c -> fmap ($ c) (rp a b)) return)

instance Alternative Fields where
  empty =
    Fields
      Ex.NoFields
      (const (const empty))
  (<|>) (Fields le lp) (Fields re rp) =
    Fields
      (Ex.EitherFields le re)
      (\ a b -> lp a b <|> rp a b)

byKeyFields :: Text -> Value a -> Fields a
byKeyFields key valueSpec =
  Fields
    (Ex.QueryFields [key] (valueExpectation valueSpec))
    parser
  where
    parser lookup _ =
      case lookup key of
        Just val ->
          Parser.atKey key $
          valueParser valueSpec val
        Nothing ->
          Parser.fail ("Key not found: " <> key)

byOneOfKeysFields :: [Text] -> Value a -> Fields a
byOneOfKeysFields keys valueSpec =
  Fields
    (Ex.QueryFields keys (valueExpectation valueSpec))
    parser
  where
    parser _ lookup =
      case lookup keys of
        Just (key, val) ->
          Parser.atKey key $
          valueParser valueSpec val
        Nothing ->
          Parser.fail ("None of the keys found: " <> showAsText keys)
