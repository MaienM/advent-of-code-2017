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
