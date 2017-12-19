module AOC11 where
import Common (numbers)
import Common.Megaparsec (Parser, parseE', symbol, (<||>))
import Control.Applicative ((<*), (*>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-- Data structure that stores the movements
data Movement = Movement {
   northWest :: Int,
   north :: Int,
   northEast :: Int,
   southEast :: Int,
   south :: Int,
   southWest :: Int
} deriving (Show, Eq)

-- Add two movements together
add :: Movement -> Movement -> Movement
add (Movement a1 b1 c1 d1 e1 f1) (Movement a2 b2 c2 d2 e2 f2) = Movement (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2) (f1+f2)

-- Build a movement object from a list of directions
fromDirections :: [String] -> Movement
fromDirections [] = Movement 0 0 0 0 0 0
fromDirections ("nw":rest) = Movement 1 0 0 0 0 0 `add` fromDirections rest
fromDirections ("n":rest)  = Movement 0 1 0 0 0 0 `add` fromDirections rest
fromDirections ("ne":rest) = Movement 0 0 1 0 0 0 `add` fromDirections rest
fromDirections ("se":rest) = Movement 0 0 0 1 0 0 `add` fromDirections rest
fromDirections ("s":rest)  = Movement 0 0 0 0 1 0 `add` fromDirections rest
fromDirections ("sw":rest) = Movement 0 0 0 0 0 1 `add` fromDirections rest
fromDirections (_:rest) = error "Invalid direction"

-- Parse a line into a Movement
directions = ["nw", "ne", "n", "sw", "se", "s"]
matchDirection = foldl1 (P.<|>) (map symbol directions) :: Parser String
matchLine = fromDirections <$> P.sepBy matchDirection (C.char ',') :: Parser Movement
parseLine :: String -> Movement
parseLine = parseE' matchLine

-- Simpify a Movement by removing moves that cancel each other out
simplifyCancel :: Movement -> Movement
simplifyCancel mov 
   | nw > 0 && se > 0 = simplifyCancel (add mov (Movement (-1) 0 0 (-1) 0 0))
   | n > 0 && s > 0   = simplifyCancel (add mov (Movement 0 (-1) 0 0 (-1) 0))
   | ne > 0 && sw > 0 = simplifyCancel (add mov (Movement 0 0 (-1) 0 0 (-1)))
   | otherwise = mov
   where
      (Movement nw n ne se s sw) = mov

-- Simplify a Movement by merging moves that can be expressed as a single move
simplifyMerge :: Movement -> Movement
simplifyMerge mov
   | nw > 0 && ne > 0 = simplifyMerge (add mov (Movement (-1) 1 (-1) 0 0 0))
   | n > 0 && se > 0 = simplifyMerge (add mov (Movement 0 (-1) 1 (-1) 0 0))
   | ne > 0 && s > 0 = simplifyMerge (add mov (Movement 0 0 (-1) 1 (-1) 0))
   | se > 0 && sw > 0 = simplifyMerge (add mov (Movement 0 0 0 (-1) 1 (-1)))
   | s > 0 && nw > 0 = simplifyMerge (add mov (Movement (-1) 0 0 0 (-1) 1))
   | sw > 0 && n > 0 = simplifyMerge (add mov (Movement 1 (-1) 0 0 0 (-1)))
   | otherwise = mov
   where
      (Movement nw n ne se s sw) = mov

-- Simplify a Movement
simplify :: Movement -> Movement
simplify mov = simplifyCancel (simplifyMerge (mov))

-- Count the amount of steps in a Movement
count :: Movement -> Int
count mov = do
   let (Movement a b c d e f) = simplify mov
   a + b + c + d + e + f

-- Get the solution for part 1
partOne :: Movement -> Int
partOne = count

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo digits = 0

main = do
   -- input <- getContents
   input <- getLine
   let movement = parseLine input
   putStrLn (show (partOne movement))
   -- putStrLn (show (partTwo digits))
