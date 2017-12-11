module AOC05Spec where
import Test.Hspec
import AOC05 (partOne, partTwo)

main :: IO ()
main = hspec $ do
   describe "partOne" $ do
      it "returns the correct value for 0 3 0 1 -3" $ partOne [0, 3, 0, 1, -3] `shouldBe` 5

   describe "partTwo" $ do
      it "returns the correct value for 0 3 0 1 -3" $ partTwo [0, 3, 0, 1, -3] `shouldBe` 10
