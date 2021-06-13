module Lib where

import GHC.Natural
import Data.List

someFunc :: IO ()
someFunc = fizzBuzzPrint $ ListLength 30



isPalindrome :: String -> Bool
isPalindrome s = s == reverse s



newtype ListLength = ListLength Natural
  deriving (Eq, Show)

initialPositives :: ListLength -> [Natural]
initialPositives listLength = unfoldr aux (1, listLength)
  where
    aux :: (Natural, ListLength) -> Maybe (Natural, (Natural, ListLength))
    aux (acc, l@(ListLength n)) =
      if acc == 1 + n
      then Nothing
      else Just (acc, (1 + acc, l))

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

fizzBuzzDescriptions :: ListLength -> [String]
fizzBuzzDescriptions l = fmap fizzBuzzDescription (fizzBuzzList l)

putMultipleStrLn :: Show a => [a] -> IO ()
putMultipleStrLn = putStr . unlines . fmap show

fizzBuzzPrint :: ListLength -> IO ()
fizzBuzzPrint = putMultipleStrLn . fizzBuzzDescriptions
