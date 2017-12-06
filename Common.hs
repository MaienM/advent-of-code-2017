module Common (
   toDigits
) where

-- Convert a list of characters into a list of numbers
toDigits :: [Char] -> [Int]
toDigits [] = []
toDigits list = [read [x] :: Int | x <- list]
