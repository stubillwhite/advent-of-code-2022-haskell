module AdventOfCode.Day01Spec (main, spec) where

import Test.Hspec ( hspec, describe, it, shouldBe, Spec )

import AdventOfCode.Day01 ( solutionOne, solutionTwo, exampleInput )

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "solutionOne" $ do
    it "should return expected result for example input" $ do
      solutionOne exampleInput `shouldBe` 24000
  describe "solutionTwo" $ do
    it "should return correct result for example input" $ do
      solutionTwo exampleInput `shouldBe` 45000      
