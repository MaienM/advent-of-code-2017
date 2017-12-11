module AOC02Spec where

import Test.Hspec
import AOC02 (spread, divisible, partOne, partTwo)

main :: IO ()
main = hspec $ do
   describe "spread" $ do
      it "returns 0 for an empty list" $ spread [] `shouldBe` 0
      it "returns 0 for a list with one element" $ spread [1] `shouldBe` 0
      it "returns 4 for [1..5]" $ spread [1..5] `shouldBe` 4
      it "returns 4 for [5, 1]" $ spread [5, 1] `shouldBe` 4
      it "returns 4 for [3, 5, 1]" $ spread [3, 5, 1] `shouldBe` 4

   describe "divisible" $ do
      it "returns 0 for an empty list" $ divisible [] `shouldBe` 0
      it "returns 0 for a list with one element" $ divisible [1] `shouldBe` 0
      it "returns 2 for [4, 8]" $ divisible [4, 8] `shouldBe` 2
      it "returns 2 for [8, 4]" $ divisible [8, 4] `shouldBe` 2
      it "returns 2 for [6, 8, 4]" $ divisible [6, 8, 4] `shouldBe` 2

   describe "partOne" $ do
      it "returns 18 for [[5,1,9,5], [7,5,3], [2,4,6,8]]" $ partOne [[5,1,9,5], [7,5,3], [2,4,6,8]] `shouldBe` 18

   describe "partTwo" $ do
      it "returns 9 for [[5,9,2,8], [9,4,7,3], [3,8,6,5]]" $ partTwo [[5,9,2,8], [9,4,7,3], [3,8,6,5]] `shouldBe` 9
