import qualified Data.List

isValid :: [String] -> Int
isValid [] = 0
isValid (words) = if words == Data.List.nub words then 1 else 0

main' :: [String] -> Int
main' [] = 0
main' (line:rest) = (isValid (words line)) + (main' rest)

main = do
   input <- getContents
   putStrLn (show (main' (lines input)))
