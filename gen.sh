#!/bin/sh

cat <<EOF > AOC$1.hs
module AOC$1 where
import Common (numbers)

-- Get the solution for part 1
partOne :: [Int] -> Int
partOne digits = 0

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo digits = 0

main = do
   -- input <- getContents
   -- let digits = map numbers (map words (lines input))
   input <- getLine
   let digits = numbers (words input)
   putStrLn (show (partOne digits))
   putStrLn (show (partTwo digits))
EOF

cat <<EOF > AOC$1Spec.hs
module AOC$1Spec where
import Test.Hspec
import AOC$1 (partOne, partTwo)

main :: IO ()
main = hspec $ do
   describe "partOne" $ do
      it "returns the correct value for [1, 2, 3]" $ partOne [1, 2, 2] \`shouldBe\` 0

   describe "partTwo" $ do
      it "returns the correct value for [1, 2, 3]" $ partTwo [1, 2, 2] \`shouldBe\` 0
EOF
