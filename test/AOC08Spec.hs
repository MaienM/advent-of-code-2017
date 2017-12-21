module AOC08Spec where
import AOC08 (Instruction(..), Action(..), set, emptyRegisters, perform, apply, change, check, target, operation, amount, parseLine, partOne, partTwo)
import Test.Hspec

exampleInstructions = [
   Instruction (Action "b" (+) 5) (Action "a" (>) 1),
   Instruction (Action "a" (+) 1) (Action "b" (<) 5),
   Instruction (Action "c" (-) (-10)) (Action "a" (>=) 1),
   Instruction (Action "c" (+) (-20)) (Action "c" (==) 10)
   ]
exampleRegisters = (set "a" 1 . set "b" 2 . set "c" 3) emptyRegisters

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "parseLine" $ do
      it "parses change target correctly" $ (target . change) (parseLine "a inc 1 if b > 2") `shouldBe` "a"
      describe "parses change operation correctly" $ do
         describe "for inc" $ do
            it "returns the correct value for 2 inc 1" $ ((operation . change) (parseLine "a inc 1 if b > 2")) 2 1 `shouldBe` 3
            it "returns the correct value for 2 inc -1" $ ((operation . change) (parseLine "a inc 1 if b > 2")) 2 (-1) `shouldBe` 1
         describe "for dec" $ do
            it "returns the correct value for 2 dec 1" $ ((operation . change) (parseLine "a dec 1 if b > 2")) 2 1 `shouldBe` 1
            it "returns the correct value for 2 dec -1" $ ((operation . change) (parseLine "a dec 1 if b > 2")) 2 (-1) `shouldBe` 3
      it "parses change amount correctly" $ (amount . change) (parseLine "a inc 1 if b > 2") `shouldBe` 1
      it "parses check target correctly" $ (target . check) (parseLine "a inc 1 if b > 2") `shouldBe` "b"
      describe "parses check operation correctly" $ do
         describe "for ==" $ do
            it "returns the correct value for 1 == 1" $ ((operation . check) (parseLine "a inc 1 if b == 2")) 1 1 `shouldBe` True
            it "returns the correct value for 1 == 2" $ ((operation . check) (parseLine "a inc 1 if b == 2")) 1 2 `shouldBe` False
         describe "for !=" $ do
            it "returns the correct value for 1 != 1" $ ((operation . check) (parseLine "a inc 1 if b != 2")) 1 1 `shouldBe` False
            it "returns the correct value for 1 != 2" $ ((operation . check) (parseLine "a inc 1 if b != 2")) 1 2 `shouldBe` True
         describe "for >" $ do
            it "returns the correct value for 1 > 0" $ ((operation . check) (parseLine "a inc 1 if b > 2")) 1 0 `shouldBe` True
            it "returns the correct value for 1 > 1" $ ((operation . check) (parseLine "a inc 1 if b > 2")) 1 1 `shouldBe` False
            it "returns the correct value for 1 > 2" $ ((operation . check) (parseLine "a inc 1 if b > 2")) 1 2 `shouldBe` False
         describe "for >=" $ do
            it "returns the correct value for 1 >= 0" $ ((operation . check) (parseLine "a inc 1 if b >= 2")) 1 0 `shouldBe` True
            it "returns the correct value for 1 >= 1" $ ((operation . check) (parseLine "a inc 1 if b >= 2")) 1 1 `shouldBe` True
            it "returns the correct value for 1 >= 2" $ ((operation . check) (parseLine "a inc 1 if b >= 2")) 1 2 `shouldBe` False
         describe "for <" $ do
            it "returns the correct value for 1 < 0" $ ((operation . check) (parseLine "a inc 1 if b < 2")) 1 0 `shouldBe` False
            it "returns the correct value for 1 < 1" $ ((operation . check) (parseLine "a inc 1 if b < 2")) 1 1 `shouldBe` False
            it "returns the correct value for 1 < 2" $ ((operation . check) (parseLine "a inc 1 if b < 2")) 1 2 `shouldBe` True
         describe "for <=" $ do
            it "returns the correct value for 1 <= 0" $ ((operation . check) (parseLine "a inc 1 if b <= 2")) 1 0 `shouldBe` False
            it "returns the correct value for 1 <= 1" $ ((operation . check) (parseLine "a inc 1 if b <= 2")) 1 1 `shouldBe` True
            it "returns the correct value for 1 <= 2" $ ((operation . check) (parseLine "a inc 1 if b <= 2")) 1 2 `shouldBe` True
      it "parses check amount correctly" $ (amount . check) (parseLine "a inc 1 if b > 2") `shouldBe` 2

   describe "perform" $ do
      it "returns the correct value for a == 0" $ perform (Action "a" (==) 1) exampleRegisters `shouldBe` True
      it "returns the correct value for a == 3" $ perform (Action "a" (==) 3) exampleRegisters `shouldBe` False
      it "returns the correct value for a + 1" $ perform (Action "a" (+) 1) exampleRegisters `shouldBe` 2
      it "returns the correct value for a - 3" $ perform (Action "a" (-) 3) exampleRegisters `shouldBe` -2
      it "returns the correct value for nonexistent + 1" $ perform (Action "nonexistent" (+) 1) exampleRegisters `shouldBe` 1

   describe "apply" $ do
      it "applies the change when the check passes" $ apply exampleRegisters (Instruction (Action "a" (+) 2) (Action "b" (==) 2)) `shouldBe` (set "a" 3 exampleRegisters)
      it "does not apply the change when the check fails" $ apply exampleRegisters (Instruction (Action "a" (+) 2) (Action "b" (>) 2)) `shouldBe` exampleRegisters

   describe "partOne" $ do
      it "returns the correct value for the example" $ partOne exampleInstructions `shouldBe` 1

   describe "partTwo" $ do
      it "returns the correct value for the example" $ partTwo exampleInstructions `shouldBe` 10
