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
partOne jumps = jumpsToEscape (+1) (fromList jumps) 0

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo jumps = jumpsToEscape (\x -> if x >= 3 then x - 1 else x + 1) (fromList jumps) 0

main = do
   input <- getContents
   let jumps = numbers (lines input)
   putStrLn (show (partOne jumps))
   putStrLn (show (partTwo jumps))
