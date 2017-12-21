module AOC10 where
import Common (shift)
import Common.Megaparsec (Parser, parseE', symbol)
import Control.Applicative ((<*), (*>))
import Data.Bits (xor)
import Data.Char (ord)
import Data.List.Split (chunksOf)
import Numeric (showHex)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char.Lexer as L

-- Parse a line into a list of ints
matchLineOne = P.sepBy L.decimal (symbol ",") :: Parser [Int]
parseLineOne :: String -> [Int]
parseLineOne = parseE' matchLineOne

-- Parse a line into a list of bytes
parseLineTwo :: String -> [Int]
parseLineTwo = map ord

-- Reverse a portion of a list, with wrap around
reverseSlice :: Int -> Int -> [a] -> [a]
reverseSlice start len list = do
   let (a, b) = splitAt len (shift list (-start))
   shift ((reverse a) ++ b) start

-- Calculate the sparse hash
sparse' :: Int -> Int -> [a] -> [Int] -> [a]
sparse' _ _ nums [] = nums
sparse' pos skip nums (length:lengths) = sparse' (pos+skip+length) (skip+1) (reverseSlice pos length nums) lengths
sparse :: [Int] -> [Int] -> [Int]
sparse = sparse' 0 0

-- Convert the sparse hash to the dense hash
dense :: [Int] -> [Int]
dense sparse = map (foldl xor 0) (chunksOf 16 sparse)

-- Convert a hash into a hexadecimal string
hexToTwoChar :: Int -> String
hexToTwoChar byte
   | byte < 16 = '0':((showHex byte) "")
   | otherwise = (showHex byte) ""
hex :: [Int] -> String
hex hash = concat (map hexToTwoChar hash)

-- Get the solution for part 1
partOne' :: [Int] -> [Int] -> Int
partOne' nums lengths = do
   let (x:y:_) = sparse nums lengths
   x * y
partOne :: [Int] -> Int
partOne = partOne' [0..255]

-- Get the solution for part 2
partTwo :: [Int] -> String
partTwo lengths = do
   let lengths' = concat (replicate 64 (lengths ++ [17, 31, 73, 47, 23]))
   let sparse' = sparse [0..255] lengths'
   let dense' = dense sparse'
   hex dense'

main = do
   input <- getLine
   putStrLn (show (partOne (parseLineOne input)))
   putStrLn (partTwo (parseLineTwo input))
