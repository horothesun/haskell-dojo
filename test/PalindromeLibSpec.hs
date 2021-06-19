module PalindromeLibSpec where

import Test.Hspec
import PalindromeLib

spec :: Spec
spec = describe "isPalindrome" $ do
  it "of \"ciao\" is False" $
    isPalindrome "ciao" `shouldBe` False

  it "of \"\" is True" $
    isPalindrome "" `shouldBe` True

  it "of \"noon\" is True" $
    isPalindrome "noon" `shouldBe` True

  it "of \"radar\" is True" $
    isPalindrome "radar" `shouldBe` True
