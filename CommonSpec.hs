module CommonSpec where
import Test.Hspec
import Common (numbers, orelse)

main :: IO ()
main = hspec $ do
   describe "numbers" $ do
      it "returns an empty list when given an empty list" $ numbers [] `shouldBe` []
      it "works for single-digit numbers" $ numbers ["1", "2", "3"] `shouldBe` [1, 2, 3]
      it "works for multi-digit numbers" $ numbers ["11", "22", "33"] `shouldBe` [11, 22, 33]
      it "works for a mixture of single-digit and multi-digit numbers" $ numbers ["11", "2", "33"] `shouldBe` [11, 2, 33]

   describe "orelse" $ do
      describe "for ints" $ do
         it "considers 0 to be falsy" $ 0 `orelse` 1 `shouldBe` (1 :: Int)
         it "considers 1 to be truthy" $ 1 `orelse` 3 `shouldBe` (1 :: Int)
         it "is chainable" $ 0 `orelse` 1 `orelse` 3 `shouldBe` (1 :: Int)
      describe "for strings" $ do
         it "considers an empty string to be falsy" $ "" `orelse` "foo" `shouldBe` "foo"
         it "considers a non-empty string to be truthy" $ "foo" `orelse` "bar" `shouldBe` "foo"
         it "is chainable" $ "" `orelse` "foo" `orelse` "bar" `shouldBe` "foo"
      describe "for lists" $ do
         it "considers an empty list to be falsy" $ [] `orelse` [1] `shouldBe` [1]
         it "considers a non-empty list to be truthy" $ [1] `orelse` [3] `shouldBe` [1]
         it "considers a list with only falsy values to be truthy" $ [0] `orelse` [3] `shouldBe` [0]
         it "is chainable" $ [] `orelse` [1] `orelse` [3] `shouldBe` [1]
