module AOC07Spec where
import AOC07 (Program(..), PTree(..), parseInput, matchInput, matchLine, root, fullWeight, validateWeight, partOne, partTwo)
import Common.Tree (buildTree)
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec
import qualified Text.Megaparsec as P

exampleInput = unlines [
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

simpleInput = unlines [
   "foo (10) -> a, b, c",
   "a (10)",
   "b (11)",
   "c (10)"
   ]
simplePrograms = [
   Program "foo" 10 ["a", "b", "c"],
   Program "a" 10 [],
   Program "b" 11 [],
   Program "c" 10 []
   ]
simpleTree = buildTree name children simplePrograms

main :: IO ()
main = hspec $ do
   describe "matchLine" $ do
      it "parses a line without children" $ parseE matchLine "aaa (10)" `shouldParse` Program "aaa" 10 []
      it "parses a line with children" $ parseE matchLine "aaa (10) -> b, c, d" `shouldParse` Program "aaa" 10 ["b", "c", "d"]
      it "does not parse a line with children but without weight" $ parseE matchLine `shouldFailOn` "aaa -> b, c, d"
      it "does not parse a line without weight or children" $ parseE matchLine `shouldFailOn` "aaa"

   describe "matchInput" $ do
      it "parses the example" $ parseE matchInput simpleInput `shouldParse` simpleTree

   describe "root" $ do
      it "returns the correct value for a simple tree" $ root simplePrograms `shouldBe` "foo"

   describe "fullWeight" $ do
      it "returns the correct value for a node without children" $ fullWeight (buildTree name children [(last simplePrograms)]) `shouldBe` 10
      it "returns the correct value for a node with children" $ fullWeight simpleTree `shouldBe` 41

   describe "validateWeight" $ do
      it "returns the correct value for a simple tree" $ validateWeight simpleTree `shouldBe` (simplePrograms !! 2, 1)

   describe "partOne" $ do
      it "returns the correct value for a simple tree" $ partOne simpleTree `shouldBe` "foo"
      it "returns the correct value for the example" $ partOne (parseInput exampleInput) `shouldBe` "tknk"

   describe "partTwo" $ do
      it "returns the correct value for a simple tree" $ partTwo simpleTree `shouldBe` 10
      it "returns the correct value for the example" $ partTwo (parseInput exampleInput) `shouldBe` 60
