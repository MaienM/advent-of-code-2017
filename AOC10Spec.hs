module AOC10Spec where
import AOC10 (matchLineOne, parseLineTwo, reverseSlice, sparse, dense, hexToTwoChar, hex, partOne', partTwo)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec $ do
   describe "matchLineOne" $ do
      it "parses a comma-separated list of numbers" $ parseE matchLineOne "1, 2" `shouldParse` [1, 2]

   describe "parseLineTwo" $ do
      it "returns the correct value for the example" $ parseLineTwo "1,2,3" `shouldBe` [49, 44, 50, 44, 51]

   describe "reverseSlice" $ do
      it "returns the correct value for 0 2 [0..4]" $ reverseSlice 0 2 [0..4] `shouldBe` [1, 0, 2, 3, 4]
      it "returns the correct value for 1 2 [0..4]" $ reverseSlice 1 2 [0..4] `shouldBe` [0, 2, 1, 3, 4]
      it "returns the correct value for 2 2 [0..4]" $ reverseSlice 2 2 [0..4] `shouldBe` [0, 1, 3, 2, 4]
      it "returns the correct value for 3 2 [0..4]" $ reverseSlice 3 2 [0..4] `shouldBe` [0, 1, 2, 4, 3]
      it "returns the correct value for 4 2 [0..4]" $ reverseSlice 4 2 [0..4] `shouldBe` [4, 1, 2, 3, 0]
      it "returns the correct value for 0 3 [0..4]" $ reverseSlice 0 3 [0..4] `shouldBe` [2, 1, 0, 3, 4]
      it "returns the correct value for 1 3 [0..4]" $ reverseSlice 1 3 [0..4] `shouldBe` [0, 3, 2, 1, 4]
      it "returns the correct value for 2 3 [0..4]" $ reverseSlice 2 3 [0..4] `shouldBe` [0, 1, 4, 3, 2]
      it "returns the correct value for 3 3 [0..4]" $ reverseSlice 3 3 [0..4] `shouldBe` [3, 1, 2, 0, 4]
      it "returns the correct value for 4 3 [0..4]" $ reverseSlice 4 3 [0..4] `shouldBe` [0, 4, 2, 3, 1]

   describe "sparse'" $ do
      it "returns the correct value for the example" $ sparse [0..4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]

   describe "dense" $ do
      it "returns the correct value for the example" $ dense (concat (replicate 4 [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22])) `shouldBe` [64, 64, 64, 64]

   describe "hexToTwoChar" $ do
      it "returns the correct value for 4" $ hexToTwoChar 4 `shouldBe` "04"
      it "returns the correct value for 9" $ hexToTwoChar 9 `shouldBe` "09"
      it "returns the correct value for 10" $ hexToTwoChar 10 `shouldBe` "0a"
      it "returns the correct value for 15" $ hexToTwoChar 15 `shouldBe` "0f"
      it "returns the correct value for 16" $ hexToTwoChar 16 `shouldBe` "10"

   describe "hex" $ do
      it "returns the correct value for [64, 7, 255]" $ hex [64, 7, 255] `shouldBe` "4007ff"

   describe "partOne'" $ do
      it "returns the correct value for the example" $ partOne' [0..4] [3, 4, 1, 5] `shouldBe` 12

   describe "partTwo" $ do
      it "returns the correct value for an empty string" $ partTwo (parseLineTwo "") `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      it "returns the correct value for 'AoC 2017'" $ partTwo (parseLineTwo "AoC 2017") `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      it "returns the correct value for '1,2,3'" $ partTwo (parseLineTwo "1,2,3") `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      it "returns the correct value for '1,2,4'" $ partTwo (parseLineTwo "1,2,4") `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
