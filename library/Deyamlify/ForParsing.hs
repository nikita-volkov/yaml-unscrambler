module Deyamlify.ForParsing
where

import Deyamlify.Prelude
import Deyamlify.Model
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Text.Megaparsec as Megaparsec
import qualified Data.HashMap.Strict as HashMap
import qualified Deyamlify.Util.HashMap as HashMap
import qualified Attoparsec.Data as AttoparsecData


vectorOfAssocs :: StringParser key -> ValueParser val -> (key -> val -> assoc) -> MappingParser (Vector assoc)
vectorOfAssocs =
  error "TODO"
