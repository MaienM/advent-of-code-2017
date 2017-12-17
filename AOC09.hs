module AOC09 where
import Common.Megaparsec (Parser, parse, (<||>), symbolAngles, symbolBraces, symbolComma)
import Control.Applicative ((<*), (*>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Type representing a node in the input
data Node = Group { children :: [Node] } | Garbage { contents :: String } deriving (Show, Eq)

-- Parse a line into a Group
matchEscape = C.char '!' *> C.anyChar :: Parser Char
matchGarbage = symbolAngles (Garbage <$> P.many (matchEscape <||> (C.notChar '>'))) :: Parser Node
matchGroup = symbolBraces (Group <$> P.sepBy matchNode symbolComma) :: Parser Node
matchNode = matchGroup <||> matchGarbage :: Parser Node
parseLine :: String -> Node
parseLine = parse matchGroup

-- Calculate the score of a Node + all nested Nodes
score :: Int -> Node -> Int
score l (Group children) = l + sum (map (score (l+1)) children)
score _ _ = 0

-- Get the solution for part 1
partOne :: Node -> Int
partOne group = score 1 group

-- Get the solution for part 2
partTwo :: Node -> Int
partTwo group = 0

main = do
   input <- getLine
   let group = parse matchGroup input
   putStrLn (show (partOne group))
   putStrLn (show (partTwo group))