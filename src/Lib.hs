module Lib where

import GHC.Natural
import Text.Read
import System.IO

program :: IO ()
program = fizzBuzzProgram



isPalindrome :: String -> Bool
isPalindrome s = s == reverse s



newtype ListLength = ListLength Natural
  deriving (Eq, Show)


initialPositives :: ListLength -> [Int]
initialPositives (ListLength n) = [1..naturalToInt n]

-- import Data.List
-- initialPositives :: ListLength -> [Int]
-- initialPositives listLength = unfoldr aux (1, listLength)
--   where
--     aux :: (Int, ListLength) -> Maybe (Int, (Int, ListLength))
--     aux (acc, l@(ListLength n)) =
--       if acc == 1 + naturalToInt n
--       then Nothing
--       else Just (acc, (1 + acc, l))

data FizzBuzz =
    Fizz Int
  | Buzz Int
  | FizzBuzz Int
  | Regular Int
  deriving (Eq, Show)

fizzBuzz :: Int -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz n
  | n `mod` 3 == 0                   = Fizz n
  | n `mod` 5 == 0                   = Buzz n
  | otherwise                        = Regular n

fizzBuzzList :: ListLength -> [FizzBuzz]
fizzBuzzList l = fizzBuzz <$> initialPositives l

fizzBuzzDescription :: FizzBuzz -> String
fizzBuzzDescription fb = case fb of
  Fizz _     -> "Fizz"
  Buzz _     -> "Buzz"
  FizzBuzz _ -> "FizzBuzz!"
  Regular n  -> show n

fizzBuzzDescriptions :: ListLength -> [String]
fizzBuzzDescriptions l = fizzBuzzDescription <$> fizzBuzzList l

putMultipleStrLn :: Show a => [a] -> IO ()
putMultipleStrLn = putStr . unlines . fmap show

readMaybeNatural :: String -> Maybe Natural
readMaybeNatural = readMaybe

fizzBuzzProgram :: IO ()
fizzBuzzProgram = do
  putStr "Insert the list length: "
  hFlush stdout
  s <- getLine
  putStrLn "Let's go!"
  case readMaybeNatural s of
    Nothing -> putStrLn $ "Error: can't parse Natural from \"" ++ s ++ "\""
    Just n  -> (putMultipleStrLn . fizzBuzzDescriptions . ListLength) n
