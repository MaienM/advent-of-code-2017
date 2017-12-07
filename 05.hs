import qualified Data.Sequence

-- Given the current jumplist and index, determine the amount of jumps to escape
jumpsToEscape :: Data.Sequence.Seq Int -> Int -> Int
jumpsToEscape list idx
   | length list <= idx = 0
   | otherwise = do
      let jump = Data.Sequence.index list idx
      1 + jumpsToEscape (Data.Sequence.update idx (jump + 1) list) (idx + jump)

main = do
   input <- getContents
   let jumps = [read line :: Int | line <- (lines input)]
   putStrLn (show (jumpsToEscape (Data.Sequence.fromList jumps) 0))
