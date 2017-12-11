module AOC07 where
import Data.Char (isSpace, isDigit, isLetter)
import Data.List ((\\), intercalate)

-- Parse a list of comma-separated things into an array
parseCommaList :: String -> [String]
parseCommaList "" = []
parseCommaList str = do
   let (word, rest) = span (/=',') str
   let strip s = dropWhile (==' ') (reverse (dropWhile (==' ') (reverse s)))
   (strip word):(parseCommaList (strip (dropWhile (==',') rest)))

-- Parse a line of the input into name, weight, 'children'
parseLine :: String -> (String, Int, [String])
parseLine line = do
   let (name, restName) = break isSpace line
   let (number, restNumber) = span isDigit (dropWhile (not . isDigit) restName)
   let children = parseCommaList (dropWhile (not . isLetter) restNumber)
   (name, read number :: Int, children)

-- Get the solution for part 1
partOne :: [String] -> String
partOne programs = do
   let processed = map parseLine programs
   let names = map (\(n, w, c) -> n) processed
   let nonRoot = concat (map (\(n, w, c) -> c) processed)
   head (names \\ nonRoot)

-- Get the solution for part 2
partTwo :: [String] -> Int
partTwo lines = 0

main = do
   input <- getContents
   let programs = lines input
   putStrLn (partOne programs)
   putStrLn (show (partTwo programs))
