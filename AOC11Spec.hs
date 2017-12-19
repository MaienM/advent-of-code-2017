module AOC11Spec where
import AOC11 (Movement(..), add, matchLine, simplifyCancel, simplifyMerge, partOne, partTwo)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec $ do
   describe "add" $ do
      it "returns the correct value for (1 2 3 4 5 6) (10 20 30 40 50 60)" $ add (Movement 1 2 3 4 5 6) (Movement 10 20 30 40 50 60) `shouldBe` Movement  11 22 33 44 55 66

   describe "matchLine" $ do
      it "parses a list of movements" $ parseE matchLine "n,nw,nw,s,s,ne,nw,n" `shouldParse` Movement 3 2 1 0 2 0
      it "does not parse an invalid movement" $ parseE matchLine `shouldFailOn` "n,se,e"

   describe "simplifyCancel" $ do
      it "returns the correct value for (3 0 0 1 0 0)" $ simplifyCancel (Movement 3 0 0 1 0 0) `shouldBe` Movement 2 0 0 0 0 0
      it "returns the correct value for (0 3 0 0 1 0)" $ simplifyCancel (Movement 0 3 0 0 1 0) `shouldBe` Movement 0 2 0 0 0 0
      it "returns the correct value for (0 0 3 0 0 1)" $ simplifyCancel (Movement 0 0 3 0 0 1) `shouldBe` Movement 0 0 2 0 0 0
      it "returns the correct value for (1 0 0 3 0 0)" $ simplifyCancel (Movement 1 0 0 3 0 0) `shouldBe` Movement 0 0 0 2 0 0
      it "returns the correct value for (0 1 0 0 3 0)" $ simplifyCancel (Movement 0 1 0 0 3 0) `shouldBe` Movement 0 0 0 0 2 0
      it "returns the correct value for (0 0 1 0 0 3)" $ simplifyCancel (Movement 0 0 1 0 0 3) `shouldBe` Movement 0 0 0 0 0 2
      it "returns the correct value for (2 6 3 4 1 5)" $ simplifyCancel (Movement 2 6 3 4 1 5) `shouldBe` Movement 0 5 0 2 0 2

   describe "simplifyMerge" $ do
      it "returns the correct value for (3 0 1 0 0 0)" $ simplifyMerge (Movement 3 0 1 0 0 0) `shouldBe` Movement 2 1 0 0 0 0
      it "returns the correct value for (0 3 0 1 0 0)" $ simplifyMerge (Movement 0 3 0 1 0 0) `shouldBe` Movement 0 2 1 0 0 0
      it "returns the correct value for (0 0 3 0 1 0)" $ simplifyMerge (Movement 0 0 3 0 1 0) `shouldBe` Movement 0 0 2 1 0 0
      it "returns the correct value for (0 0 0 3 0 1)" $ simplifyMerge (Movement 0 0 0 3 0 1) `shouldBe` Movement 0 0 0 2 1 0
      it "returns the correct value for (1 0 0 0 3 0)" $ simplifyMerge (Movement 1 0 0 0 3 0) `shouldBe` Movement 0 0 0 0 2 1
      it "returns the correct value for (0 1 0 0 0 3)" $ simplifyMerge (Movement 0 1 0 0 0 3) `shouldBe` Movement 1 0 0 0 0 2
      it "returns the correct value for (2 6 3 4 1 5)" $ simplifyMerge (Movement 2 6 3 4 1 5) `shouldBe` Movement 0 3 0 0 0 0

   describe "partOne" $ do
      it "returns the correct value for ne,ne,ne" $ partOne (Movement 0 0 3 0 0 0) `shouldBe` 3
      it "returns the correct value for ne,ne,sw,sw" $ partOne (Movement 0 0 2 0 0 2) `shouldBe` 0
      it "returns the correct value for ne,ne,s,s" $ partOne (Movement 0 0 2 0 2 0) `shouldBe` 2
      it "returns the correct value for se,sw,se,sw,sw" $ partOne (Movement 0 0 0 2 0 3) `shouldBe` 3

   describe "partTwo" $ do
      it "returns the correct value for [1, 2, 3]" $ partTwo [1, 2, 2] `shouldBe` 0
