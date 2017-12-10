-- Shift all items in a list by a given amount, wrapping around to the first position for the items at the end
shift :: [a] -> Int -> [a]
shift [] _ = []
shift list n = do
   let (a, b) = splitAt ((length list) - n) list
   b ++ a

-- Get all numbers of tuples that contain the same numbers
matching :: [(Int, Int)] -> [Int]
matching [] = []
matching list = [x | (x, y) <- list, x == y]

main = do
   input <- getLine
   let digits = [read [c] :: Int | c <- input]
   putStrLn (show (sum (matching (zip digits (shift digits 1)))))
   putStrLn (show (sum (matching (zip digits (shift digits (div (length digits) 2))))))
