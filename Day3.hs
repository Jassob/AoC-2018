-- | Solves the problems of day 3 of Advent of Code 2018
module Main where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict       as M
import           Data.HashSet                   ( HashSet )
import qualified Data.HashSet              as S

import Lib

main :: IO ()
main = do
   args <- parseArgs
   putStrLn =<< main' args

main' :: StandardArgs -> IO String
main' (StdArgs fp p)
   | p == 1 = show . part1 <$> claims
   | otherwise = show . part2 <$> claims
   where claims = map read . lines <$> readFile fp

type ClaimId = Int
type Coord = (Int, Int)

data Claim = Claim
  { cId :: ClaimId
  , coords :: [Coord]
  }

-- Reads a claim on format #cId @ mLeft,mTop: wxh
instance Read Claim where
  readsPrec p ('#':str) =
    let [(num, r1)] = readsPrec p str
        [(mLeft, r2)] = readsPrec p $ dropChar r1
        [(mTop, r3)] = readsPrec p $ dropChar r2
        [(w, 'x':r4)] = readsPrec p $ dropChar r3
        [(h, r5)] = readsPrec p r4
        size = [ (x, y) | x <- [mLeft..mLeft + w - 1], y <- [mTop..mTop + h - 1]]
    in [(Claim num size, r5)]
       where dropChar :: String -> String
             dropChar s = let [(_, rest)] = lex s in rest
  readsPrec _ _ = []

-- Shows a claim in the same format as read parses
instance Show Claim where
  show (Claim i cs) =
    let mLeft = fst $ head cs
        mTop = snd $ head cs
        h = length $ filter ((==mLeft) . fst) cs
        w = length $ filter ((==mTop) . snd) cs
    in "#" ++ show i ++ " @ " ++ show mLeft ++ "," ++ show mTop ++ ": " ++ show w ++ "x" ++ show h

part1 :: [Claim] -> Int
part1 = length . filter (>1) . M.elems . foldr go M.empty
  where go :: Claim -> HashMap Coord Int -> HashMap Coord Int
        go c m = foldr (M.alter (pure . maybe 1 (+1))) m (coords c)

part2 :: [Claim] -> [Int]
part2 cs = S.toList $ M.foldl' (foldr (S.delete)) s overlaps
  where
    overlaps = M.filter ((>1) . length) m
    (m, s) = foldr go (M.empty, S.empty) cs

    go :: Claim -> (HashMap Coord [ClaimId], HashSet ClaimId) -> (HashMap Coord [ClaimId], HashSet ClaimId)
    go c (m', s')= foldr (go' $ cId c) (m', s') (coords c)

    go' :: ClaimId -> Coord -> (HashMap Coord [ClaimId], HashSet ClaimId) -> (HashMap Coord [ClaimId], HashSet ClaimId)
    go' cId' crd (m', s') = (M.alter (pure . maybe [cId'] (cId':)) crd m', S.insert cId' s')
