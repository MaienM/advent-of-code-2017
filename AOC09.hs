module AOC09 where
import Common.Megaparsec (Parser, parseE', (<||>))
import Control.Applicative ((<*), (*>))
import Data.Either (rights)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-- Type representing a node in the input
data Node = Group { children :: [Node] } | Garbage { contents :: String } deriving (Show, Eq)

-- Parse a line into a Group
matchEscape = C.char '!' *> C.anyChar :: Parser Char
matchGarbage = C.char '<' *> (Garbage <$> rights <$> P.many (P.eitherP matchEscape (C.notChar '>'))) <* C.char '>' :: Parser Node
matchGroup = C.char '{' *> (Group <$> P.sepBy matchNode (C.char ',')) <* C.char '}' :: Parser Node
matchNode = matchGroup <||> matchGarbage :: Parser Node
parseLine :: String -> Node
parseLine = parseE' matchGroup

-- Calculate the score of a Group + all nested Groups
score :: Int -> Node -> Int
score l (Group children) = l + sum (map (score (l+1)) children)
score _ _ = 0

-- Get the length of all garbage in a Group + all nested Groups
garbage :: Node -> Int
garbage (Group children) = sum (map garbage children)
garbage (Garbage contents) = length contents

-- Get the solution for part 1
partOne :: Node -> Int
partOne group = score 1 group

-- Get the solution for part 2
partTwo :: Node -> Int
partTwo = garbage

main = do
   input <- getLine
   let group = parseLine input
   putStrLn (show (partOne group))
   putStrLn (show (partTwo group))
