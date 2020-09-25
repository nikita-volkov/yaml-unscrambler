module YamlUnscrambler.Util.Yaml
where

import YamlUnscrambler.Prelude
import qualified Conduit
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import qualified Data.Yaml.Parser as YamlParser
import qualified Data.Text.Encoding as Text
import qualified Text.Libyaml as Libyaml


parseByteStringToAeson :: ByteString -> Either Text Aeson.Value
parseByteStringToAeson input =
  first (fromString . Yaml.prettyPrintParseException) (Yaml.decodeEither' input)

parseByteStringToRawDoc :: ByteString -> Either Text YamlParser.RawDoc
parseByteStringToRawDoc input =
  first showAsText $
  unsafePerformIO $
  try @SomeException $
  Conduit.runConduitRes $
  Conduit.fuse (Libyaml.decode input) (YamlParser.sinkRawDoc)
