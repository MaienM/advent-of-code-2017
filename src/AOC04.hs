module AOC04 where
import Data.List (nub, sort)

-- Check whether a given list has duplicates
hasDuplicates :: [String] -> Bool
hasDuplicates [] = False
hasDuplicates (words) = words /= nub words

-- Get the solution for part 1
partOne :: [[String]] -> Int
partOne phrases = length (filter (not . hasDuplicates) phrases)

-- Get the solution for part 2
partTwo :: [[String]] -> Int
partTwo phrases = length (filter (not . hasDuplicates) (map (map sort) phrases))

main = do
   input <- getContents
   let phrases = map words (lines input)
   putStrLn (show (partOne phrases))
   putStrLn (show (partTwo phrases))
