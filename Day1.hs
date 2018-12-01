-- | Solves first day in Advent of Code 2018
module Main where

import           Text.Read                      ( readEither )

import Lib

main :: IO ()
main = do
  args <- parseArgs
  print =<< main' args

main' :: StandardArgs -> IO Int
main' (StdArgs fp p)
  | p == 1 = part1 <$> parsedNums
  | otherwise = undefined
  where parsedNums = map readWithSign . lines <$> readFile fp

readWithSign :: String -> Int
readWithSign [] = error "Can't read empty string"
readWithSign (s:num)
  | s == '+' = failOnParseError num
  | otherwise = negate $ failOnParseError num
    where
      failOnParseError :: Read a => String -> a
      failOnParseError str
        = either (error $ "Failed to read: " ++ str)
          id
          (readEither str)

-- | Sums all elements in the list. Caution: may overflow
part1 :: [Int] -> Int
part1 = sum

