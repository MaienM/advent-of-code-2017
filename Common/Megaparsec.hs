module Common.Megaparsec where

import Data.Void (Void)
import Control.Applicative (empty)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Parser for string
type Parser = P.Parsec Void String

-- Whitespace consumer
spaceConsumer :: Parser ()
spaceConsumer = L.space C.space1 empty empty

-- Lexeme
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

-- Symbol
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- Some common symbols
symbolParens = P.between (symbol "(") (symbol ")")
symbolBraces = P.between (symbol "{") (symbol "}")
symbolAngles = P.between (symbol "<") (symbol ">")
symbolBrackets = P.between (symbol "[") (symbol "]")
symbolSemicolon = symbol ";"
symbolComma = symbol ","
symbolColon = symbol ":"
symbolDot = symbol "."
symbolArrow = symbol "->"

-- A version of <|> that doesn't care whether the first parser consumed any input before failing
(<||>) :: Parser a -> Parser a -> Parser a
p <||> q = P.try p P.<|> q

-- For these exercises error handling isn't really that important, so simplify parsing by ignoring this
parse :: Parser a -> String -> a
parse parser line = do
   let (Right result) = P.parse parser "" line
   result

