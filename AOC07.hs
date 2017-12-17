{-# LANGUAGE TemplateHaskell #-}

module AOC07 where
import Common.Megaparsec (Parser, parse, lexeme, symbolParens, symbolArrow, symbolComma)
import Control.Applicative ((<*), (*>))
import Data.Char (isSpace, isDigit, isLetter)
import Data.Function.Memoize (memoize, deriveMemoizableParams)
import Data.List ((\\), intercalate, elemIndices)
import Data.Map (Map)
import qualified Data.Map
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

-- Type representing a single program in the tree
data Program = Program { name :: String, weight :: Int, children :: [String] } deriving (Show, Eq)
deriveMemoizableParams ''Program [0]

-- Type representing a (sub)tree, with the program information + access to the subtrees
data Tree = Tree { program :: Program, subTrees :: [Tree] } deriving (Show, Eq)
deriveMemoizableParams ''Tree [0]

-- Parse a line into a Program
matchName = lexeme $ P.some C.letterChar :: Parser String
matchWeight = lexeme $ symbolParens L.decimal :: Parser Int
matchChildList = symbolArrow *> P.sepBy1 matchName symbolComma :: Parser [String]
matchLine :: Parser Program
matchLine = do
   name <- matchName
   weight <- matchWeight
   children <- P.option [] matchChildList
   return $ Program name weight children
parseLine :: String -> Program
parseLine = parse matchLine

-- Given a list of programs, get the root name
root :: [Program] -> String
root programs = do
   let names = map name programs
   let nonRoot = concat (map children programs)
   head (names \\ nonRoot)

-- Process a list of programs into a tree
toTree' :: Map String Program -> String -> Tree
toTree' programs name = do
   let program = programs Data.Map.! name
   Tree program (map (toTree' programs) (children program))
toTree :: [Program] -> Tree
toTree programs = do
   let mapped = Data.Map.fromList (map (\x -> (name x, x)) programs)
   toTree' mapped (root programs)

-- Get the weight of a (sub)tree
fullWeight' :: Tree -> Int
fullWeight' tree = weight (program tree) + sum (map fullWeight (subTrees tree))
fullWeight :: Tree -> Int
fullWeight = memoize fullWeight'

-- Validate the weight of the given tree. Returns the program that has an incorrect weight, and by how much the weight
-- is off.
validateWeight' :: Tree -> Int -> (Program, Int)
validateWeight' tree off = do
   let trees = subTrees tree
   let weights = map fullWeight trees
   let matchingFirst = elemIndices (head weights) weights
   -- If the subtrees are all the same, that means the current program is the wrong one
   if (length matchingFirst) == (length weights)
   then (program tree, off)
   else do
      let zipped = (zip trees weights)
      let (_, cweight) = if (length matchingFirst) == 1 then (last zipped) else (head zipped)
      let (wtree, wweight) = head [(t, w) | (t, w) <- zipped, w /= cweight]
      validateWeight' wtree (wweight - cweight)
validateWeight :: Tree -> (Program, Int)
validateWeight tree = validateWeight' tree 0

-- Get the solution for part 1
partOne :: [Program] -> String
partOne = root

formatTree' :: Tree -> [String]
formatTree' tree = do
   let self = name (program tree) ++ " " ++ show (weight (program tree)) ++ "/" ++ show (fullWeight tree)
   let children = map ("   "++) (concat (map formatTree' (subTrees tree)))
   self:children
formatTree :: Tree -> String
formatTree tree = intercalate "\n" (formatTree' tree)

-- Get the solution for part 2
partTwo :: [Program] -> Int
partTwo programs = do
   let (program, off) = validateWeight (toTree programs)
   weight program - off

main = do
   input <- getContents
   let programs = lines input
   let processed = map parseLine programs
   putStrLn (partOne processed)
   putStrLn (show (partTwo processed))
