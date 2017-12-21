module Common.TreeSpec where
import Common.Tree (buildTree)
import Control.Exception (evaluate)
import Data.Tree (rootLabel, subForest)
import Test.Hspec

data Item = Item { key :: String, children :: [String] } deriving (Show, Eq)
exampleItems = [
   Item "a" ["b", "c"],
   Item "b" [],
   Item "c" ["b", "d"],
   Item "d" ["b"]
   ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "buildTree" $ do
      it "returns a tree with the correct root for the example" $ rootLabel (buildTree key children exampleItems) `shouldBe` head exampleItems
      it "fails for a tree without a root" $ evaluate (buildTree key children [Item "a" ["b"], Item "b" ["a"]]) `shouldThrow` errorCall "Tree has no root"
      it "fails for a tree with multiple roots" $ evaluate (buildTree key children (Item "e" ["b"]:exampleItems)) `shouldThrow` errorCall "Tree has multiple roots"
