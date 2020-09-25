module YamlUnscrambler
where

import YamlUnscrambler.Prelude hiding (String)
import YamlUnscrambler.Model
import qualified YamlUnscrambler.YamlValueParser as Parser
import qualified Data.Yaml.Parser as Yaml
import qualified Text.Libyaml as Libyaml
import qualified YamlUnscrambler.Expectations as Ex
import qualified Control.Foldl as Fold
import qualified Data.HashMap.Strict as HashMap
import qualified YamlUnscrambler.Util.HashMap as HashMap
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as TextAtto
import qualified Data.Vector as Vector
import qualified YamlUnscrambler.Util.Vector as Vector
import qualified Data.Vector.Generic as GenericVector


-- * Execution
-------------------------

parseByteString :: Value a -> ByteString -> Either Text a
parseByteString value input =
  Parser.runValueParser (valueParser value) input

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

nullableValue :: [Scalar a] -> Maybe (Mapping a) -> Maybe (Sequence a) -> Value (Maybe a)
nullableValue scalars mappings sequences =
  value
    ((Nothing <$ nullScalar) : fmap (fmap Just) scalars)
    (fmap (fmap Just) mappings)
    (fmap (fmap Just) sequences)


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

vectorMapping :: String key -> Value val -> (key -> val -> assoc) -> Mapping (Vector assoc)
vectorMapping key val zip =
  Mapping
    (Ex.MonomorphicMapping (stringExpectation key) (valueExpectation val))
    (Parser.foldMapping parse Fold.vector)
  where
    parse k v =
      zip <$> stringParser key k <*> valueParser val v

byKeyMapping :: Bool -> ByKey Text a -> Mapping a
byKeyMapping caseSensitive byKey =
  Mapping expectation parse
  where
    expectation =
      Ex.ByKeyMapping (CaseSensitive caseSensitive) (byKeyExpectation byKey)
    parse input =
      if caseSensitive
        then let
          map =
            HashMap.fromList input
          lookup k =
            HashMap.lookup k map
          lookupFirst kl =
            HashMap.lookupFirst kl map
          in byKeyParser byKey lookup lookupFirst
        else let
          map =
            HashMap.fromList (fmap (first Text.toLower) input)
          lookup k =
            HashMap.lookup (Text.toLower k) map
          lookupFirst kl =
            HashMap.lookupFirst (fmap Text.toLower kl) map
          in byKeyParser byKey lookup lookupFirst


-- *
-------------------------

data Sequence a =
  Sequence {
    sequenceExpectation :: Ex.Sequence,
    sequenceParser :: [Yaml.YamlValue] -> Parser.Eff a
  }
  deriving (Functor)

foldSequence :: Fold a b -> Value a -> Sequence b
foldSequence fold value =
  Sequence
    (Ex.MonomorphicSequence (valueExpectation value))
    (Parser.foldSequence (valueParser value) fold)

vectorSequence :: (GenericVector.Vector v a) => Value a -> Sequence (v a)
vectorSequence =
  foldSequence Fold.vector

byOrderSequence :: ByOrder a -> Sequence a
byOrderSequence byOrder =
  Sequence
    (Ex.ByOrderSequence (byOrderExpectation byOrder))
    (evalStateT (byOrderParser byOrder) . (0,))

byKeySequence :: ByKey Int a -> Sequence a
byKeySequence byKey =
  Sequence expectation parse
  where
    expectation =
      Ex.ByKeySequence (byKeyExpectation byKey)
    parse input =
      let
        vector =
          Vector.fromList input
        lookup k =
          vector Vector.!? k
        lookupFirst kl =
          Vector.lookupFirst kl vector
        in byKeyParser byKey lookup lookupFirst


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

enumString :: Bool -> [(Text, a)] -> String a
enumString caseSensitive assocList =
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

data ByKey key a =
  ByKey {
    byKeyExpectation :: Ex.ByKey key,
    byKeyParser :: (key -> Maybe Yaml.YamlValue) -> ([key] -> Maybe (key, Yaml.YamlValue)) -> Parser.Eff a
  }
  deriving (Functor)

instance Applicative (ByKey key) where
  pure =
    ByKey Ex.AnyByKey . const . const . pure
  (<*>) (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.BothByKey le re)
      (\ a b -> lp a b <*> rp a b)

instance Selective (ByKey key) where
  select (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.BothByKey le re)
      (\ a b -> select (lp a b) (rp a b))

instance Alternative (ByKey key) where
  empty =
    ByKey
      Ex.NoByKey
      (const (const empty))
  (<|>) (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.EitherByKey le re)
      (\ a b -> lp a b <|> rp a b)

atByKey :: Show key => key -> Value a -> ByKey key a
atByKey key valueSpec =
  ByKey
    (Ex.LookupByKey [key] (valueExpectation valueSpec))
    parser
  where
    parser lookup _ =
      case lookup key of
        Just val ->
          Parser.atShowableKey key $
          valueParser valueSpec val
        Nothing ->
          Parser.fail ("Key not found: " <> showAsText key)

atOneOfByKey :: Show key => [key] -> Value a -> ByKey key a
atOneOfByKey keys valueSpec =
  ByKey
    (Ex.LookupByKey keys (valueExpectation valueSpec))
    parser
  where
    parser _ lookup =
      case lookup keys of
        Just (key, val) ->
          Parser.atShowableKey key $
          valueParser valueSpec val
        Nothing ->
          Parser.fail ("None of the keys found: " <> showAsText keys)


-- *
-------------------------

data ByOrder a =
  ByOrder {
    byOrderExpectation :: Ex.ByOrder,
    byOrderParser :: StateT (Int, [Yaml.YamlValue]) Parser.Eff a
  }
  deriving (Functor)

instance Applicative ByOrder where
  pure =
    ByOrder Ex.AnyByOrder . pure
  (<*>) (ByOrder le lp) (ByOrder re rp) =
    ByOrder
      (Ex.BothByOrder le re)
      (lp <*> rp)

instance Selective ByOrder where
  select (ByOrder le lp) (ByOrder re rp) =
    ByOrder
      (Ex.BothByOrder le re)
      (select lp rp)

instance Alternative ByOrder where
  empty =
    ByOrder
      Ex.NoByOrder
      empty
  (<|>) (ByOrder le lp) (ByOrder re rp) =
    ByOrder
      (Ex.EitherByOrder le re)
      (lp <|> rp)

fetchByOrder :: Value a -> ByOrder a
fetchByOrder value =
  ByOrder
    (Ex.FetchByOrder (valueExpectation value))
    parser
  where
    parser =
      do
        (offset, list) <- get
        case list of
          h : t ->
            let
              !nextOffset =
                succ offset
              in do
                put (nextOffset, t)
                lift (Parser.atIndex offset (valueParser value h))
          _ ->
            lift (Parser.fail "No elements left")
