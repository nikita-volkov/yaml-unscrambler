module YamlUnscrambler.Util.Text
where

import YamlUnscrambler.Prelude
import qualified Data.Attoparsec.Text as Atto
import qualified Attoparsec.Data as Atto


deshowIfPossible :: Text -> Text
deshowIfPossible a =
  either (const a) id (deshow a)

deshow :: Text -> Either Text Text
deshow =
  first fromString . Atto.parseOnly parser
  where
    parser =
      Atto.show <* Atto.endOfInput
