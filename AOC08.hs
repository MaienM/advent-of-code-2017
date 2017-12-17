module AOC08 where
import Data.Map (Map)
import qualified Data.Map

-- Type representing the registers
type Registers = Map String Int
get :: String -> Registers -> Int
get = Data.Map.findWithDefault 0
set :: String -> Int -> Registers -> Registers
set = Data.Map.insert
emptyRegisters :: Registers
emptyRegisters = Data.Map.empty

-- Type representing an action on a register
data Action a = Action {
   target :: String,
   operation :: Int -> Int -> a,
   amount :: Int
}

-- Type representing a full register instruction
data Instruction = Instruction {
   change :: Action Int,
   check :: Action Bool
}

-- Peform an Action
perform :: Action a -> Registers -> a
perform action registers = (operation action) (get (target action) registers) (amount action)

-- Apply an Instruction
apply :: Registers -> Instruction -> Registers
apply registers instruction = do
   if perform (check instruction) registers
   then set (target (change instruction)) (perform (change instruction) registers) registers
   else registers

-- Version of apply that, in addition to the normal apply, also keeps track of the highest value ever encountered
apply' :: Registers -> Int -> Instruction -> (Registers, Int)
apply' registers max instruction = do
   let newRegisters = apply registers instruction
   let newValue = get (target (change instruction)) newRegisters
   (newRegisters, maximum [max, newValue])

-- Parse a line into an Instruction
-- Makes the assumption that all parts are space-separated
parseLine :: String -> Instruction
parseLine line = do
   let [changeTarget, changeOp, changeAmount, "if", checkTarget, checkOp, checkAmount] = words line
   let changeOperation = case (changeOp) of
         "inc" -> (+)
         "dec" -> (-)
         _ -> error "Invalid operation"
   let checkOperation = case (checkOp) of
         "==" -> (==)
         "!=" -> (/=)
         ">" -> (>)
         ">=" -> (>=)
         "<" -> (<)
         "<=" -> (<=)
         _ -> error "Invalid comparison"
   let change = Action changeTarget changeOperation (read changeAmount)
   let check = Action checkTarget checkOperation (read checkAmount)
   Instruction change check

-- Get the solution for part 1
partOne :: [Instruction] -> Int
partOne instructions = maximum (Data.Map.elems (foldl apply emptyRegisters instructions))

-- Get the solution for part 2
partTwo :: [Instruction] -> Int
partTwo instructions = snd (foldl (uncurry apply') (emptyRegisters, 0) instructions)

main = do
   input <- getContents
   let instructions = map parseLine (lines input)
   putStrLn (show (partOne instructions))
   putStrLn (show (partTwo instructions))
