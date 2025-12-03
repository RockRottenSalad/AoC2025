module Main where

import Data.Char (ord)

inputFile = "input.txt" :: String

type Battery = Int
type Bank = [Battery]
type BatteryPair = (Battery, Battery)

batteryPairOrd :: BatteryPair -> Int
batteryPairOrd (a, b) = a*10 + b 

fromChar :: Char -> Battery
fromChar ch = 10 - ((ord '9'+1) - ord ch)

fromString :: String -> Bank
fromString = map fromChar

maxByOrd :: (a -> Int) -> [a] -> a
maxByOrd _ [] = error "Empty" 
maxByOrd ord' (x:xs) = aux xs x
    where
    aux [] candidate = candidate
    aux (x:xs) candidate
        | ord' x > ord' candidate = aux xs x
        | otherwise = aux xs candidate

bestInBank :: Bank -> BatteryPair
bestInBank bank = aux bank (0, 0)
    where
    aux [x] candidate = candidate
    aux (x:xs) candidate@(c0, c1)
        | x < c0 = aux xs candidate
        | otherwise = aux xs candidate' 
            where
            candidate' = maxByOrd batteryPairOrd $ candidate : map (x,) xs

part1 :: [Bank] -> Int
part1 banks = sum $ map (batteryPairOrd . bestInBank) banks

main :: IO()
main = do
    banks <- readFile inputFile >>= \input -> return $ map fromString (lines input)
    putStr "Part 1: "
    print $ part1 banks
