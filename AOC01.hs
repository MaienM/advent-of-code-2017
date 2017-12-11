module AOC01 where
import Common (numbers)

-- Shift all items in a list by a given amount, wrapping around to the first position for the items at the end
shift :: [a] -> Int -> [a]
shift [] _ = []
shift list n = do
   let (a, b) = splitAt (mod (-n) (length list)) list
   b ++ a

-- Get all numbers of tuples that contain the same numbers
matching :: [(Int, Int)] -> [Int]
matching [] = []
matching list = [x | (x, y) <- list, x == y]

-- Get the solution for part 1
partOne :: [Int] -> Int
partOne digits = sum (matching (zip digits (shift digits 1)))

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo digits = sum (matching (zip digits (shift digits (div (length digits) 2))))

main = do
   input <- getLine
   let digits = numbers (map (:[]) input)
   putStrLn (show (partOne digits))
   putStrLn (show (partTwo digits))
