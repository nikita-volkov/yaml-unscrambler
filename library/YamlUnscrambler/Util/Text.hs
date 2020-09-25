module YamlUnscrambler.Util.Text
where

import YamlUnscrambler.Prelude
import qualified Data.Attoparsec.Text as Atto
import qualified YamlUnscrambler.Util.TextAttoparsec as Atto


deshowIfPossible :: Text -> Text
deshowIfPossible a =
  either (const a) id (deshow a)

deshow :: Text -> Either Text Text
deshow =
  first fromString . Atto.parseOnly parser
  where
    parser =
      Atto.deshow <* Atto.endOfInput
