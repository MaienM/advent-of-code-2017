module AOC03 where
import Data.Function.Memoize (memoize)
import Data.List (sort, find)
import Data.Maybe (fromJust)

--
-- Square indexes:        Square sizes:
-- 37 36 35 34 33 32 31    7  7  7  7  7  7  7
-- 38 17 16 15 14 13 30    7  5  5  5  5  5  7
-- 39 18  5  4  3 12 29    7  5  3  3  3  5  7
-- 40 19  6  1  2 11 28    7  5  3  1  3  5  7
-- 41 20  7  8  9 10 27    7  5  3  3  3  5  7
-- 42 21 22 23 24 25 26    7  5  5  5  5  5  7
-- 43 44 45 46 47 48 49    7  7  7  7  7  7  7
--
-- Side numbers:          Side indexes:
--  1  1  1  1  1  1  0    5  4  3  2  1  0  5
--  2  1  1  1  1  0  0    0  3  2  1  0  3  4
--  2  2  1  1  0  0  0    1  0  1  0  1  2  3
--  2  2  2  1  0  0  0    2  1  0  1  0  1  2
--  2  2  2  3  3  0  0    3  2  1  0  1  0  1
--  2  2  3  3  3  3  0    4  3  0  1  2  3  0
--  2  3  3  3  3  3  3    5  0  1  2  3  4  5
--

-- Calculates the width/height of the outer square
squareSize :: Int -> Int
squareSize number = do
   let size = ceiling (sqrt (fromIntegral number))
   if (mod size 2 == 0) then size + 1 else size

-- Calculates the offset from the center for the given index
indexToOffset :: Int -> (Int, Int)
indexToOffset 1 = (0, 0)
indexToOffset idx = do
   let size = squareSize idx
   let diam = div size 2
   let sidx = (size - 2) ^ 2 + 1
   let sideNum = div (idx - sidx) (size - 1)
   let sideOffset = mod (idx - sidx) (size - 1) - (diam - 1)
   case sideNum of
      0 -> (diam, sideOffset)
      1 -> (-sideOffset, diam)
      2 -> (-diam, -sideOffset)
      3 -> (sideOffset, -diam)
      otherwise -> (0, 0)

-- Calculates the index at the given offset from the center
offsetToIndex :: (Int, Int) -> Int
offsetToIndex (0, 0) = 1
offsetToIndex (x, y) = do
   let diam = max (abs x) (abs y)
   let size = diam * 2 + 1
   let sidx = (size - 2) ^ 2 + 1
   let (sideNum, sideOffset) = case () of
         _ | diam == x && diam /= -y -> (0, y)
         _ | diam == y -> (1, -x)
         _ | diam == -x -> (2, -y)
         _ | diam == -y -> (3, x)
         _ | otherwise -> (0, 0)
   sidx + (sideNum * (size - 1)) + sideOffset + (diam - 1)

-- Get the indexes that are next to the given index (including diagonals). Only
-- returns those that are lower, as higher indexes are not relevant for this
-- program
adjacentLowerIndexes' :: Int -> [Int]
adjacentLowerIndexes' 1 = []
adjacentLowerIndexes' idx = do
   let (x, y) = indexToOffset idx
   [ offsetToIndex (x - 1, y + 1),
     offsetToIndex (x, y + 1),
     offsetToIndex (x + 1, y + 1),
     offsetToIndex (x - 1, y),
     offsetToIndex (x + 1, y),
     offsetToIndex (x - 1, y - 1),
     offsetToIndex (x, y - 1),
     offsetToIndex (x + 1, y - 1)]
adjacentLowerIndexes :: Int -> [Int]
adjacentLowerIndexes idx = filter (< idx) (adjacentLowerIndexes' idx)

-- Get the stress test value for a given index
stressValue' :: Int -> Int
stressValue' 1 = 1
stressValue' idx = sum (map stressValue (adjacentLowerIndexes idx))
stressValue :: Int -> Int
stressValue = memoize stressValue'

-- Get the solution for part 1
partOne :: Int -> Int
partOne number = do
   let (x, y) = indexToOffset number
   (abs x) + (abs y)

-- Get the solution for part 2
partTwo :: Int -> Int
partTwo number = fromJust (find (>number) (map stressValue [1..]))

main = do
   input <- getLine
   let number = read input :: Int
   putStrLn (show (partOne number))
   putStrLn (show (partTwo number))
