import qualified Data.List

-- Get the smallest and largest number in a list
minMax' :: (Int, Int) -> Int -> (Int, Int)
minMax' (min, max) x = (if x < min then x else min, if x > max then x else max)
minMax :: [Int] -> (Int, Int)
minMax [] = (0, 0)
minMax [x] = (x, x)
minMax [x, y] = minMax' (minMax' (maxBound :: Int, minBound :: Int) x) y
minMax (x:rest) = minMax' (minMax rest) x

-- Get the numbers on a line
numbers :: String -> [Int]
numbers line = [read w :: Int | w <- Data.List.words line]

-- Get the difference between two numbers
diff :: Int -> Int -> Int
diff x y = abs (x - y)

main' :: [String] -> Int
main' [] = 0
main' (line:rest) = (main' rest) + (uncurry diff (minMax (numbers line)))
main = do 
   input <- getContents
   putStrLn (show (main' (lines input)))
