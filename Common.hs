{-# LANGUAGE FlexibleInstances #-}

module Common where

-- Convert a list of strings into a list of numbers
numbers :: [String] -> [Int]
numbers list = [read str :: Int | str <- list]

-- Short circuiting || operator for non-booleans
class OrAble a where
   orelse :: a -> a -> a
   orelse a b = if truthy a then a else b
   truthy :: a -> Bool
   truthy _ = True
instance OrAble Int where
   truthy 0 = False
   truthy _ = True
instance OrAble [a] where
   truthy [] = False
   truthy _ = True

-- Shift all items in a list by a given amount, wrapping around to the first position for the items at the end
shift :: [a] -> Int -> [a]
shift [] _ = []
shift list n = do
   let (a, b) = splitAt (mod (-n) (length list)) list
   b ++ a
