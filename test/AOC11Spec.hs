module AOC11Spec where
import AOC11 (Movement(..), add, matchLine, parseLine, simplifyCancel, simplifyMerge, partOne, partTwo)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "add" $ do
      it "returns the correct value for (1 2 3 4 5 6) (10 20 30 40 50 60)" $ add (Movement 1 2 3 4 5 6) (Movement 10 20 30 40 50 60) `shouldBe` Movement  11 22 33 44 55 66

   describe "matchLine" $ do
      it "parses a list of movements" $ parseE matchLine "n,nw,nw,s,s,ne,nw,n" `shouldParse` [
         Movement 0 1 0 0 0 0,
         Movement 1 0 0 0 0 0,
         Movement 1 0 0 0 0 0,
         Movement 0 0 0 0 1 0,
         Movement 0 0 0 0 1 0,
         Movement 0 0 1 0 0 0,
         Movement 1 0 0 0 0 0,
         Movement 0 1 0 0 0 0
         ]
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
      it "returns the correct value for ne,ne,ne" $ partOne (parseLine "ne,ne,ne") `shouldBe` 3
      it "returns the correct value for ne,ne,sw,sw" $ partOne (parseLine "ne,ne,sw,sw") `shouldBe` 0
      it "returns the correct value for ne,ne,s,s" $ partOne (parseLine "ne,ne,s,s") `shouldBe` 2
      it "returns the correct value for se,sw,se,sw,sw" $ partOne (parseLine "se,sw,se,sw,sw") `shouldBe` 3

   describe "partTwo" $ do
      it "returns the correct value for ne,ne,sw,sw,se" $ partTwo (parseLine "ne,ne,sw,sw,se") `shouldBe` 2
      it "returns the correct value for ne,ne,s,s" $ partTwo (parseLine "ne,ne,s,s") `shouldBe` 2
