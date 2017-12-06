-- Return a new list with the item at index replaced
replace :: [a] -> Int -> a -> [a]
replace list idx new = [if i == idx then new else old | (old, i) <- (zip list [0..(length list)])]

-- Given the current jumplist and index, determine the amount of jumps to escape
jumpsToEscape :: [Int] -> Int -> Int
jumpsToEscape list idx
   | length list <= idx = 0
   | otherwise = do
      let jump = list!!idx
      jumpsToEscape (replace list idx (jump + 1)) (idx + jump) + 1

main = do
   input <- getContents
   let jumps = [read line :: Int | line <- (lines input)]
   putStrLn (show (jumpsToEscape jumps 0))
