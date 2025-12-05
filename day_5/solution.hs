module Main where

import qualified Interval as I
import Data.List.Split (splitOn)

inputFile = "input.txt" :: String

parseIntervals :: String -> [I.Interval]
parseIntervals x = let rangesStr = lines x in map parseInterval rangesStr
    where
    parseInterval x = (lo, hi)
        where
        (loStr:hiStr:_) = splitOn "-" x
        lo = read loStr
        hi = read hiStr

main :: IO()
main = do
    input <- readFile inputFile >>= \input -> return $ splitOn "\n\n" input

    let ids :: [Int] = map read $ lines $ last input
    let intervals = parseIntervals $ head input

    let tree = foldl I.add I.empty intervals

    let treeSol = [x | x <- ids, I.contains tree x]
    putStr "Part 1: "
    print $ length treeSol

