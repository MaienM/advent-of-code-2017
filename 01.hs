-- Move the last element of a list to the first position
wrap :: [a]  -> [a]
wrap [] = []
wrap list = (last list):(init list)

-- When given a list, remove the first consecutive instance of each item, keeping only repeated items
getRepeating :: [Int] -> [Int]
getRepeating [] = []
getRepeating list = [x | (x, y) <- (zip (wrap list) list), x == y]

main = do
   input <- getLine
   let digits = [read [c] :: Int | c <- input]
   putStrLn (show (sum (getRepeating digits)))
