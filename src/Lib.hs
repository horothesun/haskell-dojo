module Lib where

import GHC.Natural
import Positive

someFunc :: IO ()
someFunc = putStrLn "someFunc"



isPalindrome :: String -> Bool
isPalindrome s = s == reverse s



positiveToNatural :: Positive -> Natural
positiveToNatural = intToNatural . positiveToInt

initialPositives :: Natural -> [Positive]
initialPositives = reverse . initialPositivesAux
  where
    initialPositivesAux :: Natural -> [Positive]
    initialPositivesAux n
      | n == 0 = []
      | otherwise = p:ps
        where
          p = unsafeIntToPositive (naturalToInt n)
          ps = initialPositivesAux (n `minusNatural` intToNatural 1)

data FizzBuzz =
    Fizz Natural
  | Buzz Natural
  | FizzBuzz Natural
  | Regular Natural
  deriving (Eq, Show)

fizzBuzzAux :: Natural -> FizzBuzz
fizzBuzzAux n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz n
  | n `mod` 3 == 0                   = Fizz n
  | n `mod` 5 == 0                   = Buzz n
  | otherwise                        = Regular n

fizzBuzz :: Natural -> [FizzBuzz]
fizzBuzz n = fmap (fizzBuzzAux . positiveToNatural) (initialPositives n)

fizzBuzzDescription :: FizzBuzz -> String
fizzBuzzDescription x = case x of
  Fizz _     -> "Fizz!"
  Buzz _     -> "Buzz!"
  FizzBuzz _ -> "FizzBuzz!"
  Regular n  -> show n
