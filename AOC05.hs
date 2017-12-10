import Data.Sequence (Seq, index, update, fromList)

-- Given the current jumplist and index, determine the amount of jumps to escape
jumpsToEscape :: Seq Int -> Int -> Int
jumpsToEscape list idx
   | length list <= idx = 0
   | otherwise = do
      let jump = index list idx
      1 + jumpsToEscape (update idx (jump + 1) list) (idx + jump)

main = do
   input <- getContents
   let jumps = [read line :: Int | line <- (lines input)]
   putStrLn (show (jumpsToEscape (fromList jumps) 0))
