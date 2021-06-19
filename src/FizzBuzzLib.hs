module FizzBuzzLib where

import GHC.Natural
import System.IO
import Text.Read
import Data.List


data FizzBuzz
  = Fizz Int
  | Buzz Int
  | FizzBuzz Int
  | Regular Int
  deriving (Eq, Show)

fizzBuzz :: Int -> FizzBuzz
fizzBuzz n
  | n `mod` 3 == 0 && n `mod` 5 == 0 = FizzBuzz n
  | n `mod` 3 == 0 = Fizz n
  | n `mod` 5 == 0 = Buzz n
  | otherwise = Regular n

newtype ListLength = ListLength Natural
  deriving (Eq, Show)

initialPositives :: ListLength -> [Int]
initialPositives (ListLength n) = [1 .. naturalToInt n]
-- import Data.List
-- initialPositives :: ListLength -> [Int]
-- initialPositives listLength = unfoldr aux (1, listLength)
--   where
--     aux :: (Int, ListLength) -> Maybe (Int, (Int, ListLength))
--     aux (acc, l@(ListLength n)) =
--       if acc == 1 + naturalToInt n
--       then Nothing
--       else Just (acc, (1 + acc, l))

fizzBuzzList :: ListLength -> [FizzBuzz]
fizzBuzzList l = fizzBuzz <$> initialPositives l

fizzBuzzDescription :: FizzBuzz -> String
fizzBuzzDescription fb = case fb of
  Fizz _ -> "Fizz"
  Buzz _ -> "Buzz"
  FizzBuzz _ -> "FizzBuzz!"
  Regular n -> show n

fizzBuzzDescriptions :: ListLength -> [String]
fizzBuzzDescriptions l = fizzBuzzDescription <$> fizzBuzzList l

-- the only interpretation done at this level is the DSL for fizz-buzz
-- but no IO/Maybe/Either effect is present in this core
business :: ListLength -> [String]
business = fmap fizzBuzzDescription . fizzBuzzList

input :: IO String
input = do
  putStr "Insert the list length: "
  hFlush stdout -- prevents `getLine` getting executed before `putStr`
  getLine

program' :: String -> Maybe [String]
program' = fmap business . parse

program'' :: String -> String -- must run the "failure" effect (Maybe, Either, etc)
program'' s = (maybe renderError renderSuccess . program') s
  where
    renderError = "Error: can't parse Natural from \"" ++ s ++ "\""
    renderSuccess xs = "Let's go!\n" ++ intercalate "\n" xs

parse :: String -> Maybe ListLength
parse = fmap ListLength . readMaybe

output :: String -> IO ()
output = putStrLn

fizzBuzzProgram :: IO ()
fizzBuzzProgram = output . program'' =<< input
