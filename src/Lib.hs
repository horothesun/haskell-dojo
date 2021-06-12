module Lib where

import GHC.Natural
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"



isPalindrome :: String -> Bool
isPalindrome s = s == reverse s



newtype ListLength = ListLength Natural
  deriving (Eq, Show)

initialPositives :: ListLength -> [Natural]
initialPositives l = reverse $ unfoldr aux l
  where
    aux :: ListLength -> Maybe (Natural, ListLength)
    aux (ListLength n) = if n == 0 then Nothing else Just (n, ListLength nMinusOne)
      where nMinusOne = n `minusNatural` intToNatural 1

data FizzBuzz =
    Fizz Natural
  | Buzz Natural
  | FizzBuzz Natural
  | Regular Natural
  deriving (Eq, Show)

fizzBuzz :: Natural -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz n
  | n `mod` 3 == 0                   = Fizz n
  | n `mod` 5 == 0                   = Buzz n
  | otherwise                        = Regular n

fizzBuzzList :: ListLength -> [FizzBuzz]
fizzBuzzList l = fmap fizzBuzz (initialPositives l)

fizzBuzzDescription :: FizzBuzz -> String
fizzBuzzDescription fb = case fb of
  Fizz _     -> "Fizz!"
  Buzz _     -> "Buzz!"
  FizzBuzz _ -> "FizzBuzz!"
  Regular n  -> show n
