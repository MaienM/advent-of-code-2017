module AOC10Spec where
import AOC10 (matchLine, reverseSlice, hash, partOne', partTwo)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec $ do
   describe "matchLine" $ do
      it "parses a comma-separated list of numbers" $ parseE matchLine "1, 2" `shouldParse` [1, 2]

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

   describe "hash'" $ do
      it "returns the correct value for the example" $ hash [0..4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]

   describe "partOne'" $ do
      it "returns the correct value for the example" $ partOne' [0..4] [3, 4, 1, 5] `shouldBe` 12

   describe "partTwo" $ do
      it "returns the correct value for [1, 2, 3]" $ partTwo [1, 2, 2] `shouldBe` 0
