module AOC01Spec where
import Test.Hspec
import AOC01 (shift, matching, partOne, partTwo)

intShift :: [Int] -> Int -> [Int]
intShift a b = (shift a b) :: [Int]

main :: IO ()
main = hspec $ do
   describe "shift" $ do
      it "returns an empty list when given an empty list" $ intShift [] 1 `shouldBe` []
      it "returns the input when shifting by 0" $ intShift [1, 2, 3] 0 `shouldBe` [1, 2, 3]
      it "returns the input when shifting by the length of the input" $ intShift [1, 2, 3] 3 `shouldBe` [1, 2, 3]
      it "returns a shifted list when shifting by 1" $ intShift [1, 2, 3] 1 `shouldBe` [3, 1, 2]
      it "returns a shifted list when shifting by 2" $ intShift [1, 2, 3] 2 `shouldBe` [2, 3, 1]
      it "returns a shifted list when shifting by the length of the input + 1" $ intShift [1, 2, 3] 4 `shouldBe` [3, 1, 2]
      it "returns a shifted list when shifting by the length of the input + 2" $ intShift [1, 2, 3] 5 `shouldBe` [2, 3, 1]

   describe "matching" $ do
      it "returns an empty list when given an empty list" $ matching [] `shouldBe` []
      it "returns an empty list if there are no matching items" $ matching [(1, 3), (1, 2)] `shouldBe` []
      it "returns one element if there is a single matching item" $ matching [(1, 1), (1, 2)] `shouldBe` [1]
      it "returns multiple elements if there are multiple matching items" $ matching [(1, 1), (2, 2)] `shouldBe` [1, 2]

   describe "partOne" $ do
      it "returns the correct value for 1122" $ partOne [1, 1, 2, 2] `shouldBe` 3
      it "returns the correct value for 1111" $ partOne [1, 1, 1, 1] `shouldBe` 4
      it "returns the correct value for 1234" $ partOne [1, 2, 3, 4] `shouldBe` 0
      it "returns the correct value for 91212129" $ partOne [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9

   describe "partTwo" $ do
      it "returns the correct value for 1212" $ partTwo [1, 2, 1, 2] `shouldBe` 6
      it "returns the correct value for 1221" $ partTwo [1, 2, 2, 1] `shouldBe` 0
      it "returns the correct value for 123425" $ partTwo [1, 2, 3, 4, 2, 5] `shouldBe` 4
      it "returns the correct value for 123123" $ partTwo [1, 2, 3, 1, 2, 3] `shouldBe` 12
      it "returns the correct value for 12131415" $ partTwo [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4
