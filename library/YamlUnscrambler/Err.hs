module YamlUnscrambler.Err
(
  ErrAtPath(..),
  Err(..),
  atSegment,
  -- *
  MaxInputSize(..),
  Signed(..),
  NumeralSystem(..),
  CaseSensitive(..),
  -- * Reexports from \"libyaml\"
  Libyaml.Tag(..),
  Libyaml.Style(..),
)
where

import YamlUnscrambler.Prelude hiding (String)
import YamlUnscrambler.Model
import qualified YamlUnscrambler.Expectations as Ex
import qualified Text.Libyaml as Libyaml


data ErrAtPath =
  ErrAtPath [Text] Err

data Err =
  KeyErr
    Ex.String
    {-^ Key expectation. -}
    Text
    {-^ Key input. -}
    Text
    {-^ String parsing error. -}
    |
  NoneOfMappingKeysFoundErr
    (Ex.ByKey Text)
    CaseSensitive
    [Text]
    {-^ Available keys. -}
    [Text]
    {-^ Keys looked up. -}
    |
  NoneOfSequenceKeysFoundErr
    (Ex.ByKey Int)
    [Int]
    |
  ScalarErr
    [Ex.Scalar]
    {-^ Expected formats. -}
    ByteString
    {-^ Input. -}
    Libyaml.Tag
    {-^ Tag. -}
    Libyaml.Style
    {-^ Style. -}
    (Maybe Text)
    {-^ Last error. -}
    |
  UnexpectedScalarErr
    Ex.Value
    |
  UnexpectedMappingErr
    Ex.Value
    |
  UnexpectedSequenceErr
    Ex.Value
    |
  UnknownAnchorErr
    Text
    |
  NotEnoughElementsErr
    Ex.ByOrder
    Int

atSegment :: Text -> ErrAtPath -> ErrAtPath
atSegment seg (ErrAtPath path err) =
  ErrAtPath (seg : path) err
