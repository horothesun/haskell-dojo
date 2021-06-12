module Lib where

import GHC.Natural
import Positive

someFunc :: IO ()
someFunc = putStrLn "someFunc"



isPalindrome :: String -> Bool
isPalindrome s = s == reverse s



positiveToNatural :: Positive -> Natural
positiveToNatural = intToNatural . positiveToInt

newtype ListLength = ListLength Natural
  deriving (Eq, Show)

initialPositives :: ListLength -> [Positive]
initialPositives = reverse . aux
  where
    aux :: ListLength -> [Positive]
    aux (ListLength n)
      | n == 0 = []
      | otherwise = p:ps
        where
          p = unsafeIntToPositive (naturalToInt n)
          ps = aux . ListLength $ n `minusNatural` intToNatural 1

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
fizzBuzzList l = fmap (fizzBuzz . positiveToNatural) (initialPositives l)

fizzBuzzDescription :: FizzBuzz -> String
fizzBuzzDescription x = case x of
  Fizz _     -> "Fizz!"
  Buzz _     -> "Buzz!"
  FizzBuzz _ -> "FizzBuzz!"
  Regular n  -> show n
