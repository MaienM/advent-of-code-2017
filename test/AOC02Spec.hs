module AOC02Spec where
import AOC02 (spread, divisible, partOne, partTwo)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "spread" $ do
      it "returns the correct value for an empty list" $ spread [] `shouldBe` 0
      it "returns the correct value for a list with one element" $ spread [1] `shouldBe` 0
      it "returns the correct value for [1..5]" $ spread [1..5] `shouldBe` 4
      it "returns the correct value for [5, 1]" $ spread [5, 1] `shouldBe` 4
      it "returns the correct value for [3, 5, 1]" $ spread [3, 5, 1] `shouldBe` 4

   describe "divisible" $ do
      it "returns the correct value for an empty list" $ divisible [] `shouldBe` 0
      it "returns the correct value for a list with one element" $ divisible [1] `shouldBe` 0
      it "returns the correct value for [4, 8]" $ divisible [4, 8] `shouldBe` 2
      it "returns the correct value for [8, 4]" $ divisible [8, 4] `shouldBe` 2
      it "returns the correct value for [6, 8, 4]" $ divisible [6, 8, 4] `shouldBe` 2

   describe "partOne" $ do
      it "returns the correct value for [[5,1,9,5], [7,5,3], [2,4,6,8]]" $ partOne [[5,1,9,5], [7,5,3], [2,4,6,8]] `shouldBe` 18

   describe "partTwo" $ do
      it "returns the correct value for [[5,9,2,8], [9,4,7,3], [3,8,6,5]]" $ partTwo [[5,9,2,8], [9,4,7,3], [3,8,6,5]] `shouldBe` 9
