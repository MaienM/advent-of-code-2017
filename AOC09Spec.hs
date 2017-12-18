module AOC09Spec where
import Common.Megaparsec (parseE)
import Test.Hspec
import Test.Hspec.Megaparsec
import AOC09 (Node(..), matchEscape, matchGarbage, matchGroup, parseLine, partOne, partTwo)

main :: IO ()
main = hspec $ do
   describe "matchEscape" $ do
      it "parses an escaped escape character" $ parseE matchEscape "!!" `shouldParse` '!'
      it "parses an escaped group start character" $ parseE matchEscape "!{" `shouldParse` '{'
      it "parses an escaped group end character" $ parseE matchEscape "!}" `shouldParse` '}'
      it "parses an escaped garbage start character" $ parseE matchEscape "!<" `shouldParse` '<'
      it "parses an escaped garbage end character" $ parseE matchEscape "!>" `shouldParse` '>'
      it "parses an escaped normal" $ parseE matchEscape "!a" `shouldParse` 'a'
      it "does not parse a non-escaped character" $ parseE matchEscape `shouldFailOn` "aa"

   describe "matchGarbage" $ do
      it "parses an empty garbage block" $ parseE matchGarbage "<>" `shouldParse` (Garbage "")
      it "parses a garbage block with normal text" $ parseE matchGarbage "<foo>" `shouldParse` (Garbage "foo")
      it "parses a garbage block with spaces" $ parseE matchGarbage "< >" `shouldParse` (Garbage " ")
      it "parses a garbage block with spaces around text" $ parseE matchGarbage "< foo >" `shouldParse` (Garbage " foo ")
      it "parses a garbage block with an escaped end symbol" $ parseE matchGarbage "<foo!>>" `shouldParse` (Garbage "foo")
      it "does not parse a garbage block with an unescaped end symbol" $ parseE matchGarbage `shouldFailOn` "<foo>>"
      it "does not parse a garbage block with a double escaped end symbol" $ parseE matchGarbage `shouldFailOn` "<foo!!>>"

   describe "matchGroup" $ do
      it "parses an empty group" $ parseE matchGroup "{}" `shouldParse` (Group [])
      it "parses a group with a nested group" $ parseE matchGroup "{{}}" `shouldParse` (Group [Group []])
      it "parses a group with multiple nested groups" $ parseE matchGroup "{{},{}}" `shouldParse` (Group [Group [], Group []])
      it "parses a group with multiple nested sub groups" $ parseE matchGroup "{{},{{}}}" `shouldParse` (Group [Group [], Group [Group []]])
      it "parses a group with nested garbage" $ parseE matchGroup "{<>}" `shouldParse` (Group [Garbage ""])
      it "parses a group with multiple nested garbages" $ parseE matchGroup "{<>,<>}" `shouldParse` (Group [Garbage "", Garbage ""])
      it "parses a group with a mixture of nested nodes" $ parseE matchGroup "{{},<>}" `shouldParse` (Group [Group [], Garbage ""])
      it "parses a group with a mixture of nested sub nodes" $ parseE matchGroup "{{},{<>}}" `shouldParse` (Group [Group [], Group [Garbage ""]])
      it "parses a group with nested garbage with a group end symbol" $ parseE matchGroup "{<}>}" `shouldParse` (Group [Garbage "}"])
      it "does not parse a group with contained characters" $ parseE matchGroup `shouldFailOn` "{a}"
      it "does not parse a group with extra characters" $ parseE matchGroup `shouldFailOn` "{}a"

   describe "partOne" $ do
      it "returns the correct value for {}" $ partOne (parseLine "{}") `shouldBe` 1
      it "returns the correct value for {{{}}}" $ partOne (parseLine "{{{}}}") `shouldBe` 6
      it "returns the correct value for {{},{}}" $ partOne (parseLine "{{},{}}") `shouldBe` 5
      it "returns the correct value for {{{},{},{{}}}}" $ partOne (parseLine "{{{},{},{{}}}}") `shouldBe` 16
      it "returns the correct value for {<a>,<a>,<a>,<a>}" $ partOne (parseLine "{<a>,<a>,<a>,<a>}") `shouldBe` 1
      it "returns the correct value for {{<ab>},{<ab>},{<ab>},{<ab>}}" $ partOne (parseLine "{{<ab>},{<ab>},{<ab>},{<ab>}}") `shouldBe` 9
      it "returns the correct value for {{<!!>},{<!!>},{<!!>},{<!!>}}" $ partOne (parseLine "{{<!!>},{<!!>},{<!!>},{<!!>}}") `shouldBe` 9
      it "returns the correct value for {{<a!>},{<a!>},{<a!>},{<ab>}}" $ partOne (parseLine "{{<a!>},{<a!>},{<a!>},{<ab>}}") `shouldBe` 3

   describe "partTwo" $ do
      it "returns the correct value for <>" $ partTwo (parseLine "{<>}") `shouldBe` 0
      it "returns the correct value for <random characters>" $ partTwo (parseLine "{<random characters>}") `shouldBe` 17
      it "returns the correct value for <<<<>" $ partTwo (parseLine "{<<<<>}") `shouldBe` 3
      it "returns the correct value for <{!>}>" $ partTwo (parseLine "{<{!>}>}") `shouldBe` 2
      it "returns the correct value for <!!>" $ partTwo (parseLine "{<!!>}") `shouldBe` 0
      it "returns the correct value for <!!!>>" $ partTwo (parseLine "{<!!!>>}") `shouldBe` 0
      it "returns the correct value for <{o\"i!a,<{i<a>" $ partTwo (parseLine "{<{o\"i!a,<{i<a>}") `shouldBe` 10
