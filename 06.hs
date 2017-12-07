import qualified Data.List
import qualified Data.Maybe

-- Calculate the amount a bank with a certain offset from the first bank behind the one being rebalanced should get
rebalancePosition :: Int -> Int -> Int -> Int
rebalancePosition banks amount offset = ceiling ((fromIntegral (amount - offset)) / (fromIntegral banks))

-- Rebalance the banks a single cycle
rebalanceBanks :: [Int] -> [Int]
rebalanceBanks [] = []
rebalanceBanks list = do
   let len = length list
   let max = maximum list
   let idx = Data.Maybe.fromJust (Data.List.elemIndex max list)
   [(if i == idx then 0 else x) + (rebalancePosition len max (mod (i - idx - 1) len)) | (x, i) <- (zip list [0..(len - 1)])]

-- Rebalance the banks until a duplicate is encountered
rebalanceBanksUntilStable :: [[Int]] -> Int
rebalanceBanksUntilStable [] = 0
rebalanceBanksUntilStable (current:previous)
   | elem current previous = 0
   | otherwise = 1 + rebalanceBanksUntilStable ((rebalanceBanks current):current:previous)

main = do
   input <- getLine
   let banks = [read word :: Int | word <- (words input)]
   putStrLn (show (rebalanceBanksUntilStable [banks]))
