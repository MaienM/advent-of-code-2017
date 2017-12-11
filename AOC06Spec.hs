module AOC06Spec where
import Test.Hspec
import AOC06 (rebalanceBanks, rebalanceBanksUntilStable, partOne, partTwo)

main :: IO ()
main = hspec $ do
   describe "rebalanceBanks" $ do
      it "returns the correct value for [0, 2, 7, 0]" $ rebalanceBanks [0, 2, 7, 0] `shouldBe` [2, 4, 1, 2]
      it "returns the correct value for [2, 4, 1, 2]" $ rebalanceBanks [2, 4, 1, 2] `shouldBe` [3, 1, 2, 3]
      it "returns the correct value for [3, 1, 2, 3]" $ rebalanceBanks [3, 1, 2, 3] `shouldBe` [0, 2, 3, 4]
      it "returns the correct value for [0, 2, 3, 4]" $ rebalanceBanks [0, 2, 3, 4] `shouldBe` [1, 3, 4, 1]
      it "returns the correct value for [1, 3, 4, 1]" $ rebalanceBanks [1, 3, 4, 1] `shouldBe` [2, 4, 1, 2]

   describe "rebalanceBanksUntilStable" $ do
      it "returns the correct value for [[0, 2, 7, 0]]" $ rebalanceBanksUntilStable [[0, 2, 7, 0]] `shouldBe` [[2,4,1,2], [1,3,4,1], [0,2,3,4], [3,1,2,3], [2,4,1,2], [0,2,7,0]]

   describe "partOne" $ do
      it "returns the correct value for [0, 2, 7, 0]" $ partOne [0, 2, 7, 0] `shouldBe` 5

   describe "partTwo" $ do
      it "returns the correct value for [0, 2, 7, 0]" $ partTwo [0, 2, 7, 0] `shouldBe` 4
