module Deyamlify.Util.Text
where

import Deyamlify.Prelude
import qualified Data.Attoparsec.Text as Atto
import qualified Deyamlify.Util.TextAttoparsec as Atto


deshowIfPossible :: Text -> Text
deshowIfPossible a =
  either (const a) id (deshow a)

deshow :: Text -> Either Text Text
deshow =
  first fromString . Atto.parseOnly parser
  where
    parser =
      Atto.deshow <* Atto.endOfInput
