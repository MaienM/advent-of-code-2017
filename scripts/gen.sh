#!/bin/sh

src="src/AOC$1.hs"
echo "Generating source in '$src'"
cat <<EOF > "$src"
module AOC$1 where
import Common (numbers)
import Common.Megaparsec (Parser, parseE', (<||>))
import Control.Applicative ((<*), (*>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-- Data structure for data
data Data = Data String

-- Parse a line into a Data
matchData = Data <$> P.some C.anyChar :: Parser Data
parseLine :: String -> Data
parseLine = parseE' matchData

-- Get the solution for part 1
partOne :: [Int] -> Int
partOne digits = 0

-- Get the solution for part 2
partTwo :: [Int] -> Int
partTwo digits = 0

main = do
   -- input <- getContents
   input <- getLine
   let digits = numbers (words input)
   let data = parseLine input
   putStrLn (show (partOne digits))
   putStrLn (show (partTwo digits))
EOF

test="test/AOC$1Spec.hs"
echo "Generating tests in '$test'"
cat <<EOF > "$test"
module AOC$1Spec where
import AOC$1 (matchData, partOne, partTwo)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "matchData" $ do
      it "parses anything" $ parseE matchData "foo" \`shouldParse\` "foo"
      it "does not parse an empty string" $ parseE matchData \`shouldFailOn\` ""

   describe "partOne" $ do
      it "returns the correct value for [1, 2, 3]" $ partOne [1, 2, 2] \`shouldBe\` 0

   describe "partTwo" $ do
      it "returns the correct value for [1, 2, 3]" $ partTwo [1, 2, 2] \`shouldBe\` 0
EOF
