module AOC05 where
import Common (numbers)
import Data.Sequence (Seq, index, update, fromList)

-- Given the current jumplist, index, and mutator, determine the amount of jumps to escape
jumpsToEscape :: (Int -> Int) -> Seq Int -> Int -> Int
jumpsToEscape mutator list idx
   | length list <= idx = 0
   | otherwise = do
      let jump = index list idx
      1 + jumpsToEscape mutator (update idx (mutator jump) list) (idx + jump)

-- Get the solution for part 1
partOne :: [Int] -> Int
partOne digits = jumpsToEscape (+1) (fromList digits) 0

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo digits = do
   let mutator x
         | x >= 3 = x - 1
         | otherwise = x + 1
   jumpsToEscape mutator (fromList digits) 0

main = do
   input <- getContents
   let digits = numbers (lines input)
   putStrLn (show (partOne digits))
   putStrLn (show (partTwo digits))
