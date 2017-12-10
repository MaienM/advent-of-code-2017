import Data.List (sort)

-- Get the smallest and largest number in a list
minMax' :: (Int, Int) -> Int -> (Int, Int)
minMax' (min, max) x = (if x < min then x else min, if x > max then x else max)
minMax :: [Int] -> (Int, Int)
minMax [] = (0, 0)
minMax [x] = (x, x)
minMax [x, y] = minMax' (minMax' (maxBound :: Int, minBound :: Int) x) y
minMax (x:rest) = minMax' (minMax rest) x

-- Get the difference between the smallest and largest number in a list
minMaxDiff :: [Int] -> Int
minMaxDiff list = do
   let (min, max) = minMax list
   max - min

-- Or operator for ints
intOr :: Int -> Int -> Int -> Int
intOr 0 0 x = x
intOr 0 x _ = x
intOr x _ _ = x

-- Get the result of the first even division possible with the given numbers
divisible' :: [Int] -> Int
divisible' [] = 0
divisible' [x] = 0
divisible' [x, y] = if (mod x y) == 0 then (div x y) else 0
divisible' (x:y:rest) = intOr (divisible' [x, y]) (divisible (x:rest)) (divisible (y:rest))
divisible :: [Int] -> Int
divisible list = divisible' ((reverse . sort) list)

-- Get the numbers on a line
numbers :: String -> [Int]
numbers line = [read w :: Int | w <- words line]

main = do 
   input <- getContents
   putStrLn (show (sum (map (minMaxDiff . numbers) (lines input))))
   putStrLn (show (sum (map (divisible . numbers) (lines input))))
