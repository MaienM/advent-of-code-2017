module AOC02 where
import Common (numbers, orelse)
import Data.List (sort)

-- Get the difference between the smallest and largest number in a list
spread :: [Int] -> Int
spread [] = 0
spread [x] = 0
spread list = (maximum list) - (minimum list)

-- Get the result of the first even division possible with the given numbers
divisible' :: [Int] -> Int
divisible' [] = 0
divisible' [x] = 0
divisible' [x, y] = if (mod x y) == 0 then (div x y) else 0
divisible' (x:y:rest) = divisible' [x, y] `orelse` divisible (x:rest) `orelse` divisible (y:rest)
divisible :: [Int] -> Int
divisible list = divisible' ((reverse . sort) list)

-- Get the solution for part 1
partOne :: [[Int]] -> Int
partOne digits = sum (map spread digits)

-- Get the solution for part 2
partTwo :: [[Int]] -> Int
partTwo digits = sum (map divisible digits)

main = do 
   input <- getContents
   let digits = map numbers (map words (lines input))
   putStrLn (show (partOne digits))
   putStrLn (show (partTwo digits))
