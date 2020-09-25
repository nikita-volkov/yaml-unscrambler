module Deyamlify.Util.TextAttoparsec
where

import Deyamlify.Prelude
import Data.Attoparsec.Text
import qualified GHC.Show
import qualified Data.Text as Text


{-|
Parse the output of the 'show' function applied to 'String' or 'Text'
to what was used for its input.
-}
deshow :: Parser Text
deshow =
  char '"' *> body <* char '"'
  where
    body =
      mconcat <$> many chunk
      where
        chunk =
          escaped <|> nonEscaped
        nonEscaped =
          takeWhile1 (\ a -> a /= '\\' && a /= '"')
        escaped =
          Text.singleton <$> escapedChar

-- https://hackage.haskell.org/package/base-4.14.0.0/docs/src/GHC.Show.html#showLitChar
escapedChar :: Parser Char
escapedChar =
  char '\\' *> escapedCharBody

escapedCharBody :: Parser Char
escapedCharBody =
  char '\\' <|>
  char 'a' $> '\a' <|>
  char 'b' $> '\b' <|>
  char 'f' $> '\f' <|>
  char 'n' $> '\n' <|>
  char 'r' $> '\r' <|>
  char 't' $> '\t' <|>
  char 'v' $> '\v' <|>
  string "DEL" $> '\DEL' <|>
  ordEscapedCharBody <|>
  asciiTabEscapedCharBody

asciiTabEscapedCharBody :: Parser Char
asciiTabEscapedCharBody =
  zipWith asciiTabChar [0..] GHC.Show.asciiTab & asum
  where
    asciiTabChar index chars =
      string (fromString chars) $> chr index

ordEscapedCharBody :: Parser Char
ordEscapedCharBody =
  chr <$> decimal
