import Data.List (nub)

isValid :: [String] -> Int
isValid [] = 0
isValid (words) = if words == nub words then 1 else 0

main' :: [String] -> Int
main' [] = 0
main' (line:rest) = (isValid (words line)) + (main' rest)

main = do
   input <- getContents
   putStrLn (show (main' (lines input)))
