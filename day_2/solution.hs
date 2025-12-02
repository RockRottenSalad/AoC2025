module Main where

import Data.List.Split (splitOn)
import Data.Bits (shiftR)

inputFile = "input.txt" :: String

type Range = (Int, Int)

fromString :: String -> Range
fromString str = pair $ map read (splitOn "-" str)
    where
    pair (x:y:_) = (x, y)

-- You call it cheating, I call it utilizing domain knowledge
ilog10 :: Int -> Int
ilog10 x 
    | x >= 10^10 = 10
    | x >= 10^9 = 9
    | x >= 10^8 = 8
    | x >= 10^7 = 7
    | x >= 10^6 = 6
    | x >= 10^5 = 5
    | x >= 10^4 = 4
    | x >= 10^3 = 3
    | x >= 10^2 = 2
    | x >= 10^1 = 1
    | otherwise = 0

numLength :: Int -> Int
numLength x = 1 + ilog10 x

splitNum :: Int -> (Int, Int)
splitNum x = (firstHalf, secondHalf)
    where
    halfLen = numLength x `shiftR` 1
    mult = 10 ^ halfLen
    firstHalf = x `div` mult
    secondHalf = x - firstHalf * mult

isInvalid :: Int -> Bool
isInvalid x = firstHalf == secondHalf
    where
    (firstHalf, secondHalf) = splitNum x

sumInvalidIds :: Range -> Int
sumInvalidIds (lo, hi) = sum [x | x <- [lo..hi], isInvalid x]

part1 :: [Range] -> Int
part1 ranges = sum $ map sumInvalidIds ranges

splitNumN :: Int -> Int -> [String]
splitNumN x n = result
    where
    arr = show x 
    result = aux arr []
        where
        aux [] result = result
        aux xs result = let (seq, rest) = splitAt n xs in aux rest (seq:result)


isInvalid' :: Int -> Bool
isInvalid' x = any (aux . (x,)) [1..len]
    where
    len = numLength x
    aux (x, n)
        | len `mod` n /= 0 = False
        | otherwise = isRepeatingSeq $ splitNumN x n 
            where
            isRepeatingSeq [x] = False
            isRepeatingSeq (x:xs) = all (==x) xs

sumInvalidIds' :: Range -> Int
sumInvalidIds' (lo, hi) = sum [x | x <- [lo..hi], isInvalid' x]

part2 :: [Range] -> Int
part2 ranges = sum $ map sumInvalidIds' ranges

main :: IO()
main = do
    ranges <- readFile inputFile >>= \input -> return $ map fromString (splitOn "," input)
    putStr "Part 1: "
    print $ part1 ranges
    putStr "Part 2: "
    print $ part2 ranges

