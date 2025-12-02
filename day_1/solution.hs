module Main where

import Prelude hiding (Right, Left)

inputFile = "input.txt" :: String

data Rotation = Left Int | Right Int deriving Show
newtype Dial = Dial Int deriving Show
dialPositions = 100 :: Int

turn :: Dial -> Rotation -> Dial
turn (Dial position) (Right delta) = Dial ((position + delta) `mod` dialPositions)
turn (Dial position) (Left delta)  = Dial pos
    where
    diff = position - delta
    pos = if diff >= 0
          then diff 
          else (-diff * (dialPositions-1)) `mod` dialPositions

isZero :: Dial -> Bool
isZero (Dial 0) = True
isZero _        = False

fromString :: String -> Rotation
fromString ('R':xs) = Right $ read xs
fromString ('L':xs) = Left  $ read xs
fromString xs       = error $ "Invalid string: '" ++ xs ++ "'"

part1 :: Dial -> [Rotation] -> Int
part1 dial rotations = snd $ foldl accum (dial, 0) rotations
    where
    accum (dial@(Dial x), times0) rotation = (dial', times0') 
        where
        dial' = turn dial rotation
        times0' = times0 + if isZero dial' then 1 else 0


-- Like turn, but includes the number of times the dial crossed zero
-- Also counts also if the end position is zero
turn' :: Dial -> Rotation -> (Dial, Int)
turn' dial@(Dial pos) rot@(Right delta) = (turn dial rot, (pos+delta) `div` dialPositions)
turn' dial@(Dial pos) rot@(Left delta)  = (turn dial rot, times0)
    where
    diff = pos - delta
    times = if diff > 0
          then 0 
          else ((-diff) + dialPositions) `div` dialPositions
    times0 = times - if pos == 0 then 1 else 0

part2 :: Dial -> [Rotation] -> Int
part2 dial rotations = snd $ foldl accum (dial, 0) rotations
    where
    accum (dial@(Dial x), times0) rotation = (dial', times0'') 
        where
        (dial', times0') = turn' dial rotation
        times0'' = times0 + times0'

main :: IO()
main = do
    rotations <- readFile inputFile >>= \input -> return $ map fromString (lines input)

    putStr "Part 1: "
    print $ part1 (Dial 50) rotations

    putStr "Part 2: "
    print $ part2 (Dial 50) rotations

