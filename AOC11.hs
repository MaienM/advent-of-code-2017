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

-- Convert a direction to a movement
direction :: String -> Movement
direction "nw" = Movement 1 0 0 0 0 0
direction "n"  = Movement 0 1 0 0 0 0
direction "ne" = Movement 0 0 1 0 0 0
direction "se" = Movement 0 0 0 1 0 0
direction "s"  = Movement 0 0 0 0 1 0
direction "sw" = Movement 0 0 0 0 0 1
direction _ = error "Invalid direction"

-- Parse a line into a list of Movements
directions = ["nw", "ne", "n", "sw", "se", "s"]
matchDirection = direction <$> foldl1 (P.<|>) (map symbol directions) :: Parser Movement
matchLine = P.sepBy matchDirection (C.char ',') :: Parser [Movement]
parseLine :: String -> [Movement]
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
count (Movement a b c d e f) = a + b + c + d + e + f

-- Get the solution for part 1
partOne :: [Movement] -> Int
partOne movements = count (simplify (foldl1 add movements))

-- Get the solution for part 2
partTwo :: [Movement] -> Int
partTwo [] = 0
partTwo [mov] = count mov
partTwo (m1:m2:rest) = do
   let mov = simplify (add m1 m2)
   maximum [count mov, partTwo (mov:rest)]

main = do
   input <- getLine
   let movements = parseLine input
   putStrLn (show (partOne movements))
   putStrLn (show (partTwo movements))
