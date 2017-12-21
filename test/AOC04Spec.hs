module AOC04Spec where
import AOC04 (hasDuplicates, partOne, partTwo)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "hasDuplicates" $ do
      it "returns false for an empty list" $ hasDuplicates [] `shouldBe` False
      it "returns false for a list without duplicates" $ hasDuplicates ["aa", "bb"] `shouldBe` False
      it "returns true for a list with duplicates" $ hasDuplicates ["aa", "aa", "bb"] `shouldBe` True

   describe "partOne" $ do
      it "returns the correct value for aa bb cc dd ee" $ partOne [words "aa bb cc dd ee"] `shouldBe` 1
      it "returns the correct value for aa bb cc dd aa" $ partOne [words "aa bb cc dd aa"] `shouldBe` 0
      it "returns the correct value for aa bb cc dd aaa" $ partOne [words "aa bb cc dd aaa"] `shouldBe` 1

   describe "partTwo" $ do
      it "returns the correct value for abcde fghij" $ partTwo [words "abcde fghij"] `shouldBe` 1
      it "returns the correct value for abcde xyz ecdab" $ partTwo [words "abcde xyz ecdab"] `shouldBe` 0
      it "returns the correct value for a ab abc abd abf abj" $ partTwo [words "a ab abc abd abf abj"] `shouldBe` 1
      it "returns the correct value for iiii oiii ooii oooi oooo" $ partTwo [words "iiii oiii ooii oooi oooo"] `shouldBe` 1
      it "returns the correct value for oiii ioii iioi iiio" $ partTwo [words "oiii ioii iioi iiio"] `shouldBe` 0
