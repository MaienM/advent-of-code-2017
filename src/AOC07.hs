module AOC07 where
import Common.Tree (buildTree)
import Common.Megaparsec (Parser, parseE', lexeme, symbol)
import Control.Applicative ((<*), (*>))
import Data.Char (isSpace, isDigit, isLetter)
import Data.Either (lefts)
import Data.List ((\\), intercalate, elemIndices)
import Data.Tree (Tree, flatten, rootLabel, subForest)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Type representing a single program in the tree
data Program = Program { name :: String, weight :: Int, children :: [String] } deriving (Show, Eq)

-- Type representing a program tree
type PTree = Tree Program

-- Parse a line into a Program
matchName = lexeme $ P.some C.letterChar :: Parser String
matchWeight = lexeme $ symbol "(" *> L.decimal <* symbol ")" :: Parser Int
matchChildList = symbol "->" *> P.sepBy1 matchName (symbol ",") :: Parser [String]
matchLine :: Parser Program
matchLine = do
   name <- matchName
   weight <- matchWeight
   children <- P.option [] matchChildList
   return $ Program name weight children
matchInput = buildTree name children <$> lefts <$> P.sepBy1 (P.eitherP matchLine C.space) C.eol :: Parser PTree
parseInput :: String -> PTree
parseInput = parseE' matchInput

-- Given a list of programs, get the root name
root :: [Program] -> String
root programs = do
   let names = map name programs
   let nonRoot = concat (map children programs)
   head (names \\ nonRoot)

-- Get the weight of a (sub)tree
fullWeight :: PTree -> Int
fullWeight tree = weight (rootLabel tree) + sum (map fullWeight (subForest tree))

-- Validate the weight of the given tree. Returns the program that has an incorrect weight, and by how much the weight
-- is off.
validateWeight' :: PTree -> Int -> (Program, Int)
validateWeight' tree off = do
   let trees = subForest tree
   let weights = map fullWeight trees
   let matchingFirst = elemIndices (head weights) weights
   -- If the subtrees are all the same, that means the current program is the wrong one
   if (length matchingFirst) == (length weights)
   then (rootLabel tree, off)
   else do
      let zipped = (zip trees weights)
      let (_, cweight) = if (length matchingFirst) == 1 then (last zipped) else (head zipped)
      let (wtree, wweight) = head [(t, w) | (t, w) <- zipped, w /= cweight]
      validateWeight' wtree (wweight - cweight)
validateWeight :: PTree -> (Program, Int)
validateWeight tree = validateWeight' tree 0

-- Get the solution for part 1
partOne :: PTree -> String
partOne = name . rootLabel

-- Get the solution for part 2
partTwo :: PTree -> Int
partTwo tree = do
   let (program, off) = validateWeight tree
   weight program - off

main = do
   input <- getContents
   let tree = parseInput input
   putStrLn (partOne tree)
   putStrLn (show (partTwo tree))
