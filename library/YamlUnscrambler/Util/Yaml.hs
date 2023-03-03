module YamlUnscrambler.Util.Yaml where

import Conduit qualified
import Data.Yaml.Parser qualified as YamlParser
import Text.Libyaml qualified as Libyaml
import YamlUnscrambler.Prelude

parseByteStringToRawDoc :: ByteString -> Either Text YamlParser.RawDoc
parseByteStringToRawDoc input =
  first (mappend "YAML AST parsing: " . showAsText) $
    unsafePerformIO $
      try @SomeException $
        Conduit.runConduitRes $
          Conduit.fuse (Libyaml.decode input) (YamlParser.sinkRawDoc)
