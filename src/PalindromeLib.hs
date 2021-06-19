module PalindromeLib where

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
