module AOC03Spec where
import AOC03 (indexToOffset, offsetToIndex, adjacentLowerIndexes, stressValue, partOne, partTwo)
import Test.Hspec

main :: IO ()
main = hspec $ do
   describe "indexToOffset" $ do
      it "returns the correct offset for 1" $ indexToOffset 1 `shouldBe` (0, 0)
      it "returns the correct offset for 2" $ indexToOffset 2 `shouldBe` (1, 0)
      it "returns the correct offset for 3" $ indexToOffset 3 `shouldBe` (1, 1)
      it "returns the correct offset for 4" $ indexToOffset 4 `shouldBe` (0, 1)
      it "returns the correct offset for 5" $ indexToOffset 5 `shouldBe` (-1, 1)
      it "returns the correct offset for 6" $ indexToOffset 6 `shouldBe` (-1, 0)
      it "returns the correct offset for 7" $ indexToOffset 7 `shouldBe` (-1, -1)
      it "returns the correct offset for 8" $ indexToOffset 8 `shouldBe` (0, -1)
      it "returns the correct offset for 9" $ indexToOffset 9 `shouldBe` (1, -1)
      it "returns the correct offset for 10" $ indexToOffset 10 `shouldBe` (2, -1)
      it "returns the correct offset for 11" $ indexToOffset 11 `shouldBe` (2, 0)
      it "returns the correct offset for 12" $ indexToOffset 12 `shouldBe` (2, 1)
      it "returns the correct offset for 13" $ indexToOffset 13 `shouldBe` (2, 2)
      it "returns the correct offset for 14" $ indexToOffset 14 `shouldBe` (1, 2)
      it "returns the correct offset for 15" $ indexToOffset 15 `shouldBe` (0, 2)
      it "returns the correct offset for 16" $ indexToOffset 16 `shouldBe` (-1, 2)
      it "returns the correct offset for 17" $ indexToOffset 17 `shouldBe` (-2, 2)
      it "returns the correct offset for 18" $ indexToOffset 18 `shouldBe` (-2, 1)
      it "returns the correct offset for 19" $ indexToOffset 19 `shouldBe` (-2, 0)
      it "returns the correct offset for 20" $ indexToOffset 20 `shouldBe` (-2, -1)
      it "returns the correct offset for 21" $ indexToOffset 21 `shouldBe` (-2, -2)
      it "returns the correct offset for 22" $ indexToOffset 22 `shouldBe` (-1, -2)
      it "returns the correct offset for 23" $ indexToOffset 23 `shouldBe` (0, -2)
      it "returns the correct offset for 24" $ indexToOffset 24 `shouldBe` (1, -2)
      it "returns the correct offset for 25" $ indexToOffset 25 `shouldBe` (2, -2)
      it "returns the correct offset for 26" $ indexToOffset 26 `shouldBe` (3, -2)
      it "returns the correct offset for 27" $ indexToOffset 27 `shouldBe` (3, -1)
      it "returns the correct offset for 28" $ indexToOffset 28 `shouldBe` (3, 0)
      it "returns the correct offset for 29" $ indexToOffset 29 `shouldBe` (3, 1)
      it "returns the correct offset for 30" $ indexToOffset 30 `shouldBe` (3, 2)
      it "returns the correct offset for 31" $ indexToOffset 31 `shouldBe` (3, 3)
      it "returns the correct offset for 32" $ indexToOffset 32 `shouldBe` (2, 3)
      it "returns the correct offset for 33" $ indexToOffset 33 `shouldBe` (1, 3)
      it "returns the correct offset for 34" $ indexToOffset 34 `shouldBe` (0, 3)
      it "returns the correct offset for 35" $ indexToOffset 35 `shouldBe` (-1, 3)
      it "returns the correct offset for 36" $ indexToOffset 36 `shouldBe` (-2, 3)
      it "returns the correct offset for 37" $ indexToOffset 37 `shouldBe` (-3, 3)
      it "returns the correct offset for 38" $ indexToOffset 38 `shouldBe` (-3, 2)
      it "returns the correct offset for 39" $ indexToOffset 39 `shouldBe` (-3, 1)
      it "returns the correct offset for 40" $ indexToOffset 40 `shouldBe` (-3, 0)
      it "returns the correct offset for 41" $ indexToOffset 41 `shouldBe` (-3, -1)
      it "returns the correct offset for 42" $ indexToOffset 42 `shouldBe` (-3, -2)
      it "returns the correct offset for 43" $ indexToOffset 43 `shouldBe` (-3, -3)
      it "returns the correct offset for 44" $ indexToOffset 44 `shouldBe` (-2, -3)
      it "returns the correct offset for 45" $ indexToOffset 45 `shouldBe` (-1, -3)
      it "returns the correct offset for 46" $ indexToOffset 46 `shouldBe` (0, -3)
      it "returns the correct offset for 47" $ indexToOffset 47 `shouldBe` (1, -3)
      it "returns the correct offset for 48" $ indexToOffset 48 `shouldBe` (2, -3)
      it "returns the correct offset for 49" $ indexToOffset 49 `shouldBe` (3, -3)

   describe "offsetToIndex" $ do
      it "returns the correct index for (0, 0)" $ offsetToIndex (0, 0) `shouldBe` 1
      it "returns the correct index for (1, 0)" $ offsetToIndex (1, 0) `shouldBe` 2
      it "returns the correct index for (1, 1)" $ offsetToIndex (1, 1) `shouldBe` 3
      it "returns the correct index for (0, 1)" $ offsetToIndex (0, 1) `shouldBe` 4
      it "returns the correct index for (-1, 1)" $ offsetToIndex (-1, 1) `shouldBe` 5
      it "returns the correct index for (-1, 0)" $ offsetToIndex (-1, 0) `shouldBe` 6
      it "returns the correct index for (-1, -1)" $ offsetToIndex (-1, -1) `shouldBe` 7
      it "returns the correct index for (0, -1)" $ offsetToIndex (0, -1) `shouldBe` 8
      it "returns the correct index for (1, -1)" $ offsetToIndex (1, -1) `shouldBe` 9
      it "returns the correct index for (2, -1)" $ offsetToIndex (2, -1) `shouldBe` 10
      it "returns the correct index for (2, 0)" $ offsetToIndex (2, 0) `shouldBe` 11
      it "returns the correct index for (2, 1)" $ offsetToIndex (2, 1) `shouldBe` 12
      it "returns the correct index for (2, 2)" $ offsetToIndex (2, 2) `shouldBe` 13
      it "returns the correct index for (1, 2)" $ offsetToIndex (1, 2) `shouldBe` 14
      it "returns the correct index for (0, 2)" $ offsetToIndex (0, 2) `shouldBe` 15
      it "returns the correct index for (-1, 2)" $ offsetToIndex (-1, 2) `shouldBe` 16
      it "returns the correct index for (-2, 2)" $ offsetToIndex (-2, 2) `shouldBe` 17
      it "returns the correct index for (-2, 1)" $ offsetToIndex (-2, 1) `shouldBe` 18
      it "returns the correct index for (-2, 0)" $ offsetToIndex (-2, 0) `shouldBe` 19
      it "returns the correct index for (-2, -1)" $ offsetToIndex (-2, -1) `shouldBe` 20
      it "returns the correct index for (-2, -2)" $ offsetToIndex (-2, -2) `shouldBe` 21
      it "returns the correct index for (-1, -2)" $ offsetToIndex (-1, -2) `shouldBe` 22
      it "returns the correct index for (0, -2)" $ offsetToIndex (0, -2) `shouldBe` 23
      it "returns the correct index for (1, -2)" $ offsetToIndex (1, -2) `shouldBe` 24
      it "returns the correct index for (2, -2)" $ offsetToIndex (2, -2) `shouldBe` 25
      it "returns the correct index for (3, -2)" $ offsetToIndex (3, -2) `shouldBe` 26
      it "returns the correct index for (3, -1)" $ offsetToIndex (3, -1) `shouldBe` 27
      it "returns the correct index for (3, 0)" $ offsetToIndex (3, 0) `shouldBe` 28
      it "returns the correct index for (3, 1)" $ offsetToIndex (3, 1) `shouldBe` 29
      it "returns the correct index for (3, 2)" $ offsetToIndex (3, 2) `shouldBe` 30
      it "returns the correct index for (3, 3)" $ offsetToIndex (3, 3) `shouldBe` 31
      it "returns the correct index for (2, 3)" $ offsetToIndex (2, 3) `shouldBe` 32
      it "returns the correct index for (1, 3)" $ offsetToIndex (1, 3) `shouldBe` 33
      it "returns the correct index for (0, 3)" $ offsetToIndex (0, 3) `shouldBe` 34
      it "returns the correct index for (-1, 3)" $ offsetToIndex (-1, 3) `shouldBe` 35
      it "returns the correct index for (-2, 3)" $ offsetToIndex (-2, 3) `shouldBe` 36
      it "returns the correct index for (-3, 3)" $ offsetToIndex (-3, 3) `shouldBe` 37
      it "returns the correct index for (-3, 2)" $ offsetToIndex (-3, 2) `shouldBe` 38
      it "returns the correct index for (-3, 1)" $ offsetToIndex (-3, 1) `shouldBe` 39
      it "returns the correct index for (-3, 0)" $ offsetToIndex (-3, 0) `shouldBe` 40
      it "returns the correct index for (-3, -1)" $ offsetToIndex (-3, -1) `shouldBe` 41
      it "returns the correct index for (-3, -2)" $ offsetToIndex (-3, -2) `shouldBe` 42
      it "returns the correct index for (-3, -3)" $ offsetToIndex (-3, -3) `shouldBe` 43
      it "returns the correct index for (-2, -3)" $ offsetToIndex (-2, -3) `shouldBe` 44
      it "returns the correct index for (-1, -3)" $ offsetToIndex (-1, -3) `shouldBe` 45
      it "returns the correct index for (0, -3)" $ offsetToIndex (0, -3) `shouldBe` 46
      it "returns the correct index for (1, -3)" $ offsetToIndex (1, -3) `shouldBe` 47
      it "returns the correct index for (2, -3)" $ offsetToIndex (2, -3) `shouldBe` 48
      it "returns the correct index for (3, -3)" $ offsetToIndex (3, -3) `shouldBe` 49

   describe "adjacentLowerIndexes" $ do
      it "returns the correct indexes for 1" $ adjacentLowerIndexes 1 `shouldMatchList` []
      it "returns the correct indexes for 2" $ adjacentLowerIndexes 2 `shouldMatchList` [1]
      it "returns the correct indexes for 3" $ adjacentLowerIndexes 3 `shouldMatchList` [1, 2]
      it "returns the correct indexes for 4" $ adjacentLowerIndexes 4 `shouldMatchList` [1, 2, 3]
      it "returns the correct indexes for 5" $ adjacentLowerIndexes 5 `shouldMatchList` [1, 4]
      it "returns the correct indexes for 6" $ adjacentLowerIndexes 6 `shouldMatchList` [1, 4, 5]
      it "returns the correct indexes for 7" $ adjacentLowerIndexes 7 `shouldMatchList` [1, 6]
      it "returns the correct indexes for 8" $ adjacentLowerIndexes 8 `shouldMatchList` [1, 2, 6, 7]
      it "returns the correct indexes for 9" $ adjacentLowerIndexes 9 `shouldMatchList` [1, 2, 8]
      it "returns the correct indexes for 10" $ adjacentLowerIndexes 10 `shouldMatchList` [2, 9]
      it "returns the correct indexes for 11" $ adjacentLowerIndexes 11 `shouldMatchList` [2, 3, 9, 10]
      it "returns the correct indexes for 12" $ adjacentLowerIndexes 12 `shouldMatchList` [2, 3, 11]
      it "returns the correct indexes for 13" $ adjacentLowerIndexes 13 `shouldMatchList` [3, 12]
      it "returns the correct indexes for 14" $ adjacentLowerIndexes 14 `shouldMatchList` [3, 4, 12, 13]
      it "returns the correct indexes for 15" $ adjacentLowerIndexes 15 `shouldMatchList` [3, 4, 5, 14]
      it "returns the correct indexes for 16" $ adjacentLowerIndexes 16 `shouldMatchList` [4, 5, 15]
      it "returns the correct indexes for 17" $ adjacentLowerIndexes 17 `shouldMatchList` [5, 16]
      it "returns the correct indexes for 18" $ adjacentLowerIndexes 18 `shouldMatchList` [5, 6, 16, 17]
      it "returns the correct indexes for 19" $ adjacentLowerIndexes 19 `shouldMatchList` [5, 6, 7, 18]
      it "returns the correct indexes for 20" $ adjacentLowerIndexes 20 `shouldMatchList` [6, 7, 19]
      it "returns the correct indexes for 21" $ adjacentLowerIndexes 21 `shouldMatchList` [7, 20]
      it "returns the correct indexes for 22" $ adjacentLowerIndexes 22 `shouldMatchList` [7, 8, 20, 21]
      it "returns the correct indexes for 23" $ adjacentLowerIndexes 23 `shouldMatchList` [7, 8, 9, 22]
      it "returns the correct indexes for 24" $ adjacentLowerIndexes 24 `shouldMatchList` [8, 9, 10, 23]
      it "returns the correct indexes for 25" $ adjacentLowerIndexes 25 `shouldMatchList` [9, 10, 24]
      it "returns the correct indexes for 26" $ adjacentLowerIndexes 26 `shouldMatchList` [10, 25]
      it "returns the correct indexes for 27" $ adjacentLowerIndexes 27 `shouldMatchList` [10, 11, 25, 26]
      it "returns the correct indexes for 30" $ adjacentLowerIndexes 30 `shouldMatchList` [12, 13, 29]
      it "returns the correct indexes for 31" $ adjacentLowerIndexes 31 `shouldMatchList` [13, 30]
      it "returns the correct indexes for 32" $ adjacentLowerIndexes 32 `shouldMatchList` [13, 14, 30, 31]
      it "returns the correct indexes for 36" $ adjacentLowerIndexes 36 `shouldMatchList` [16, 17, 35]
      it "returns the correct indexes for 37" $ adjacentLowerIndexes 37 `shouldMatchList` [17, 36]
      it "returns the correct indexes for 38" $ adjacentLowerIndexes 38 `shouldMatchList` [17, 18, 36, 37]
      it "returns the correct indexes for 48" $ adjacentLowerIndexes 48 `shouldMatchList` [24, 25, 26, 47]
      it "returns the correct indexes for 49" $ adjacentLowerIndexes 49 `shouldMatchList` [25, 26, 48]

   describe "stressValue" $ do
      it "returns the correct value for 1" $ stressValue 1 `shouldBe` 1
      it "returns the correct value for 2" $ stressValue 2 `shouldBe` 1
      it "returns the correct value for 3" $ stressValue 3 `shouldBe` 2
      it "returns the correct value for 4" $ stressValue 4 `shouldBe` 4
      it "returns the correct value for 5" $ stressValue 5 `shouldBe` 5
      it "returns the correct value for 6" $ stressValue 6 `shouldBe` 10
      it "returns the correct value for 7" $ stressValue 7 `shouldBe` 11
      it "returns the correct value for 8" $ stressValue 8 `shouldBe` 23
      it "returns the correct value for 9" $ stressValue 9 `shouldBe` 25
      it "returns the correct value for 10" $ stressValue 10 `shouldBe` 26
      it "returns the correct value for 11" $ stressValue 11 `shouldBe` 54
      it "returns the correct value for 12" $ stressValue 12 `shouldBe` 57
      it "returns the correct value for 13" $ stressValue 13 `shouldBe` 59
      it "returns the correct value for 14" $ stressValue 14 `shouldBe` 122
      it "returns the correct value for 15" $ stressValue 15 `shouldBe` 133
      it "returns the correct value for 16" $ stressValue 16 `shouldBe` 142
      it "returns the correct value for 17" $ stressValue 17 `shouldBe` 147
      it "returns the correct value for 18" $ stressValue 18 `shouldBe` 304
      it "returns the correct value for 19" $ stressValue 19 `shouldBe` 330
      it "returns the correct value for 20" $ stressValue 20 `shouldBe` 351
      it "returns the correct value for 21" $ stressValue 21 `shouldBe` 362
      it "returns the correct value for 22" $ stressValue 22 `shouldBe` 747
      it "returns the correct value for 23" $ stressValue 23 `shouldBe` 806
      it "returns the correct value for 24" $ stressValue 24 `shouldBe` 880
      it "returns the correct value for 25" $ stressValue 25 `shouldBe` 931

   describe "partOne" $ do
      it "returns the correct value for 1" $ partOne 1 `shouldBe` 0
      it "returns the correct value for 12" $ partOne 12 `shouldBe` 3
      it "returns the correct value for 23" $ partOne 23 `shouldBe` 2
      it "returns the correct value for 1024" $ partOne 1024 `shouldBe` 31

   describe "partTwo" $ do
      it "returns the correct value for 100" $ partTwo 100 `shouldBe` 122
      it "returns the correct value for 1000" $ partTwo 1000 `shouldBe` 1968
      it "returns the correct value for 10000" $ partTwo 10000 `shouldBe` 13486
