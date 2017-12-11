module AOC06 where
import Common (numbers)
import Data.Function.Memoize (memoize)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Calculate the amount a bank with a certain offset from the first bank behind the one being rebalanced should get
rebalancePosition :: Int -> Int -> Int -> Int
rebalancePosition banks amount offset = ceiling ((fromIntegral (amount - offset)) / (fromIntegral banks))

-- Rebalance the banks a single cycle
rebalanceBanks :: [Int] -> [Int]
rebalanceBanks [] = []
rebalanceBanks list = do
   let len = length list
   let max = maximum list
   let idx = fromJust (elemIndex max list)
   [(if i == idx then 0 else x) + (rebalancePosition len max (mod (i - idx - 1) len)) | (x, i) <- (zip list [0..(len - 1)])]

-- Rebalance the banks until a duplicate is encountered
-- Returns the stack of all iterations gone through, including the first duplicate
rebalanceBanksUntilStable' :: [[Int]] -> [[Int]]
rebalanceBanksUntilStable' [] = []
rebalanceBanksUntilStable' (current:previous)
   | elem current previous = current:previous
   | otherwise = rebalanceBanksUntilStable' ((rebalanceBanks current):current:previous)
rebalanceBanksUntilStable :: [[Int]] -> [[Int]]
rebalanceBanksUntilStable = memoize rebalanceBanksUntilStable'

-- Get the solution for part 1
partOne :: [Int] -> Int
partOne banks = length (rebalanceBanksUntilStable [banks]) - 1

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo banks = do
   let dup:stack = rebalanceBanksUntilStable [banks]
   fromJust (elemIndex dup stack) + 1

main = do
   input <- getLine
   let banks = numbers (words input)
   putStrLn (show (partOne banks))
   putStrLn (show (partTwo banks))
