module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = describe "isPalindrome" $ do

  it "of \"ciao\" returns false" $
    isPalindrome "ciao" `shouldBe` False

  it "of \"\" returns true" $
    isPalindrome "" `shouldBe` True

  it "of \"noon\" returns true" $
    isPalindrome "noon" `shouldBe` True

  it "of \"radar\" returns true" $
    isPalindrome "radar" `shouldBe` True
