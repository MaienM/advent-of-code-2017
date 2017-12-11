module AOC07Spec where
import Test.Hspec
import AOC07 (parseCommaList, parseLine, partOne, partTwo)

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

main :: IO ()
main = hspec $ do
   describe "parseCommaList" $ do
      it "returns the correct value for an empty string" $ parseCommaList "" `shouldBe` []
      it "returns the correct value for a" $ parseCommaList "a" `shouldBe` ["a"]
      it "returns the correct value for a,bee, c" $ parseCommaList "a,bee, c" `shouldBe` ["a", "bee", "c"]

   describe "parseLine" $ do
      it "returns the correct value for aaa (10)" $ parseLine "aaa (10)" `shouldBe` ("aaa", 10, [])
      it "returns the correct value for aaa (10) -> b, c, d" $ parseLine "aaa (10) -> b, c, d" `shouldBe` ("aaa", 10, ["b", "c", "d"])

   describe "partOne" $ do
      it "returns the correct value for the example" $ partOne exampleInput `shouldBe` "tknk"

   describe "partTwo" $ do
      it "returns the correct value for [1, 2, 3]" $ partTwo ["a", "b", "c"] `shouldBe` 0
