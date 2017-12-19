module AOC10 where
import Common (shift)
import Common.Megaparsec (Parser, parseE', symbol)
import Control.Applicative ((<*), (*>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

-- Parse a line into a list of ints
matchLine = P.sepBy L.decimal (symbol ",") :: Parser [Int]
parseLine :: String -> [Int]
parseLine = parseE' matchLine

-- Reverse a portion of a list, with wrap around
reverseSlice :: Int -> Int -> [a] -> [a]
reverseSlice start len list = do
   let (a, b) = splitAt len (shift list (-start))
   shift ((reverse a) ++ b) start

-- Apply the 'hash'
hash' :: Int -> Int -> [a] -> [Int] -> [a]
hash' _ _ nums [] = nums
hash' pos skip nums (length:lengths) = hash' (pos+skip+length) (skip+1) (reverseSlice pos length nums) lengths
hash :: [Int] -> [Int] -> [Int]
hash = hash' 0 0

-- Get the solution for part 1
partOne' :: [Int] -> [Int] -> Int
partOne' lengths nums = do
   let (x:y:_) = hash lengths nums
   x * y
partOne :: [Int] -> Int
partOne = partOne' [0..255]

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo lengths = 0

main = do
   -- input <- getContents
   input <- getLine
   let lengths = parseLine input
   putStrLn (show (partOne lengths))
   putStrLn (show (partTwo lengths))
