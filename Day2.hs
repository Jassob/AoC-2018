-- | Solves the second day of Advent of Code 2018
module Main where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict       as M
import           Data.Bool                      ( bool )

import Lib

main :: IO ()
main = do
  args <- parseArgs
  putStrLn =<< main' args

main' :: StandardArgs -> IO String
main' (StdArgs fp p)
  | p == 1 = show . part1 <$> input
  | otherwise = unlines . part2 <$> input
  where input = lines <$> readFile fp

part1 :: [String] -> Int
part1 ss = uncurry (*) $ foldr go (0, 0) ss
  where go :: String -> (Int, Int) -> (Int, Int)
        go line (twice, thrice) = (upd2 twice, upd3 thrice)
          where
            occurrences = M.elems $ count line
            upd2 = bool (+1) id . null $ filter (==2) occurrences
            upd3 = bool (+1) id . null $ filter (==3) occurrences

        count :: String -> HashMap Char Int
        count = foldr (M.alter $ pure . maybe 1 (+1)) M.empty

part2 :: [String] -> [String]
part2 = map (uncurry commonString . snd) . filter ((==1) . fst) . go
  where
    go :: [String] -> [(Int, (String, String))]
    go ls = do
      line1 <- ls
      line2 <- ls
      pure (differences line1 line2, (line1, line2))

    differences :: String -> String -> Int
    differences as = length . filter id . zipWith (/=) as

    commonString :: String -> String -> String
    commonString s1 = map snd . filter fst . zipWith (\a b -> (a == b, a)) s1
