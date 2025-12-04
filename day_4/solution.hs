module Main where

import qualified Data.Set as S

inputFile = "input.txt" :: String

data Tile = Empty | Paper deriving (Show, Eq)

fromChar :: Char -> Tile
fromChar '.' = Empty
fromChar '@' = Paper

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (y, x) = [(y+1,x), (y-1,x),
                   (y,x+1), (y,x-1),
                   (y+1,x+1), (y+1,x-1),
                   (y-1,x+1), (y-1,x-1)]

countAdjacent :: (Int, Int) -> S.Set (Int, Int) -> Int
countAdjacent p@(y, x) m = length $ filter (`S.member` m) $ adjacent p

parse :: String -> S.Set (Int, Int)
parse str = foldl accum S.empty $ do
        (y, line) <- zip [0..] $ lines str
        (x, ch) <- zip [0..] $ map fromChar line
        [((y, x), ch)]
        where
        accum m (_, Empty) = m
        accum m (k, Paper) = S.insert k m

findAccessible :: S.Set (Int, Int) -> S.Set (Int, Int)
findAccessible m = S.filter (\k -> countAdjacent k m < 4) m

part1 :: S.Set (Int, Int) -> Int
part1 m = length $ findAccessible m

part2 :: S.Set (Int, Int) -> Int
part2 m = aux 0 m
    where
    aux count m = let
            accessible = findAccessible m
            m' = S.difference m accessible
        in if accessible /= S.empty
        then aux (count + length accessible) m'
        else count

main :: IO()
main = do
    input <- readFile inputFile >>= \input -> return $ parse input
    print $ part1 input
    print $ part2 input
