module Common.Megaparsec where

import Data.Void (Void)
import Control.Applicative (empty, (<*))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error (parseErrorPretty')

-- Parser for string
type Parser = P.Parsec Void String

-- Whitespace consumer
spaceConsumer :: Parser ()
spaceConsumer = L.space (P.skipSome (C.char ' ')) empty empty

-- Lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Symbol
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- A version of <|> that doesn't care whether the first parser consumed any input before failing
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = P.try p P.<|> q

-- A version of parse that requires consuming all input, and that removes the name field
parseE :: Parser a -> String -> Either (P.ParseError (P.Token String) Void) a
parseE p i = P.parse (p <* P.eof) "" i

-- A version of parseE that errors out on failures
parseE' :: Parser a -> String -> a
parseE' parser input =
   case (parseE parser input) of
      Left err -> error (parseErrorPretty' input err)
      Right result -> result
