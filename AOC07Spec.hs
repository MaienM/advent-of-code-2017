module AOC07Spec where
import AOC07 (Program(Program), Tree(Tree), parseLine, root, toTree, fullWeight, validateWeight, partOne, partTwo)
import Test.Hspec

exampleInput = [
   "pbga (66)",
   "xhth (57)",
   "ebii (61)",
   "havc (66)",
   "ktlj (57)",
   "fwft (72) -> ktlj, cntj, xhth",
   "qoyq (66)",
   "padx (45) -> pbga, havc, qoyq",
   "tknk (41) -> ugml, padx, fwft",
   "jptl (61)",
   "ugml (68) -> gyxo, ebii, jptl",
   "gyxo (61)",
   "cntj (57)"
   ]
examplePrograms = [
   Program "foo" 10 ["a", "b", "c"],
   Program "a" 10 [],
   Program "b" 11 [],
   Program "c" 10 []
   ]
exampleTree = Tree (examplePrograms !! 0) [
   Tree (examplePrograms !! 1) [],
   Tree (examplePrograms !! 2) [],
   Tree (examplePrograms !! 3) []
   ]

main :: IO ()
main = hspec $ do
   describe "parseLine" $ do
      it "returns the correct value for aaa (10)" $ parseLine "aaa (10)" `shouldBe` Program "aaa" 10 []
      it "returns the correct value for aaa (10) -> b, c, d" $ parseLine "aaa (10) -> b, c, d" `shouldBe` Program "aaa" 10 ["b", "c", "d"]

   describe "root" $ do
      it "returns the correct value for a simple tree" $ root examplePrograms `shouldBe` "foo"

   describe "toTree" $ do
      it "returns the correct value for a simple tree" $ toTree examplePrograms `shouldBe` exampleTree

   describe "fullWeight" $ do
      it "returns the correct value for a node without children" $ fullWeight (Tree (examplePrograms !! 0) []) `shouldBe` 10
      it "returns the correct value for a node with children" $ fullWeight exampleTree `shouldBe` 41

   describe "validateWeight" $ do
      it "returns the correct value for a simple tree" $ validateWeight exampleTree `shouldBe` (examplePrograms !! 2, 1)

   describe "partOne" $ do
      it "returns the correct value for a simple tree" $ partOne examplePrograms `shouldBe` "foo"
      it "returns the correct value for the example" $ partOne (map parseLine exampleInput) `shouldBe` "tknk"

   describe "partTwo" $ do
      it "returns the correct value for a simple tree" $ partTwo examplePrograms `shouldBe` 10
      it "returns the correct value for the example" $ partTwo (map parseLine exampleInput) `shouldBe` 60
