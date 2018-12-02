-- | Solves first day in Advent of Code 2018
module Main where

import           Control.Monad                  ( foldM )
import           Control.Monad.State            ( StateT
                                                , gets
                                                , modify
                                                , evalStateT )
import           Control.Monad.Trans.Except     ( ExceptT
                                                , runExceptT
                                                , throwE )
import           Data.Bool                      ( bool )
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet              as S
import           Text.Read                      ( readEither )

import Lib

main :: IO ()
main = do
  args <- parseArgs
  print =<< main' args

main' :: StandardArgs -> IO Int
main' (StdArgs fp p)
  | p == 1 = part1 <$> parsedNums
  | otherwise = part2 =<< parsedNums
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

-- PART 1
-- | Sums all elements in the list. Caution: may overflow
part1 :: [Int] -> Int
part1 = sum

-- PART 2
part2 :: [Int] -> IO Int
part2 is = do
  res <- evalStateT (runExceptT $ foldM go 0 (concat $ repeat is)) S.empty
  either pure (fail s) res
  where
    s = "Could not find a repeating frequency"

    go :: Int -> Int -> ExceptT Int (StateT (HashSet Int) IO) Int
    go acc num = let cur = acc + num in
      gets (S.member cur) >>= bool (modify (S.insert cur) >> pure cur) (throwE cur)
