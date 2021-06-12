module Positive (intToMaybePositive, unsafeIntToPositive, Positive(positiveToInt)) where

newtype Positive = Positive { positiveToInt :: Int }
  deriving (Eq, Ord)

instance Show Positive where
  show = show . positiveToInt

intToMaybePositive :: Int -> Maybe Positive
intToMaybePositive i = if i < 0 then Nothing else Just (Positive i)

unsafeIntToPositive :: Int -> Positive
unsafeIntToPositive i
  | i < 1 = error "Non-positive Int can't be converted to Positive"
  | otherwise = Positive i
