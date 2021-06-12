module LibSpec where

import GHC.Natural
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Lib
import Positive

instance Arbitrary Natural where
  arbitrary = arbitrarySizedNatural
  shrink    = shrinkIntegral

instance Arbitrary ListLength where
  arbitrary = ListLength <$> arbitrary

spec :: Spec
spec = describe "All Lib functions" $ do

  describe "isPalindrome" $ do
    it "of \"ciao\" is False" $
      isPalindrome "ciao" `shouldBe` False

    it "of \"\" is True" $
      isPalindrome "" `shouldBe` True

    it "of \"noon\" is True" $
      isPalindrome "noon" `shouldBe` True

    it "of \"radar\" is True" $
      isPalindrome "radar" `shouldBe` True


  describe "initialPositives" $ do
    prop "output's sorted in ascending order" $
      \n ->
        let outs = initialPositives n
        in outs == sort outs

    prop "output's length is the input value" $
      \n -> length (initialPositives $ ListLength n) == naturalToInt n

    prop "output's sum is n * ( n + 1 ) / 2" $
      \n ->
        let r = toRational n
        in toRational (sum $ fmap positiveToInt (initialPositives $ ListLength n)) == r * (r + 1) / 2


  describe "fizzBuzz" $ do
    it "of 1 is Regular 1" $
      fizzBuzz 1 `shouldBe` Regular 1

    it "of 3 is Fizz 3" $
      fizzBuzz 3 `shouldBe` Fizz 3

    it "of 5 is Fizz 5" $
      fizzBuzz 5 `shouldBe` Buzz 5

    it "of 15 is FizzBuzz 15" $
      fizzBuzz 15 `shouldBe` FizzBuzz 15
