module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s
