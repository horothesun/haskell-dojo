module LibSpec where

import GHC.Natural
import Data.List
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Lib


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
    prop "output's elements are all > 0" $
      \l -> all (> 0) $ initialPositives l

    prop "output's sorted in ascending order" $
      \l ->
        let outs = initialPositives l
        in outs == sort outs

    prop "output's length is the input value" $
      \n -> length (initialPositives $ ListLength n) == naturalToInt n

    prop "output's sum is n * ( n + 1 ) / 2" $
      \n ->
        let r = toRational n
        in toRational (sum $ initialPositives $ ListLength n) == r * (r + 1) / 2


  describe "fizzBuzz" $ do
    it "of 1 is Regular 1" $
      fizzBuzz 1 `shouldBe` Regular 1

    it "of 3 is Fizz 3" $
      fizzBuzz 3 `shouldBe` Fizz 3

    it "of 5 is Fizz 5" $
      fizzBuzz 5 `shouldBe` Buzz 5

    it "of 15 is FizzBuzz 15" $
      fizzBuzz 15 `shouldBe` FizzBuzz 15


  describe "fizzBuzzList" $ do
    it "of ListLength 15 is [Regular 1, Regular 2, Fizz 3, Regular 4, Buzz 5, Fizz 6, ..., FizzBuzz 15]" $
      fizzBuzzList (ListLength 15) `shouldBe`
        [
          Regular 1, Regular 2, Fizz 3, Regular 4, Buzz 5,
          Fizz 6, Regular 7, Regular 8, Fizz 9, Buzz 10,
          Regular 11, Fizz 12, Regular 13, Regular 14, FizzBuzz 15
        ]


  describe "fizzBuzzDescription" $ do
    prop "of (Fizz n) is \"Fizz!\"" $
      \n -> fizzBuzzDescription (Fizz n) == "Fizz"

    prop "of (Buzz n) is \"Buzz!\"" $
      \n -> fizzBuzzDescription (Buzz n) == "Buzz"

    prop "of (FizzBuzz n) is \"FizzBuzz!\"" $
      \n -> fizzBuzzDescription (FizzBuzz n) == "FizzBuzz!"

    it "of (Regular 8) is \"8\"" $
      fizzBuzzDescription (Regular 8) `shouldBe` "8"
