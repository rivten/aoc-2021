module Main where

import Data.List.Split

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

sumOfFuelToGetTo :: [Int] -> Int -> Int
sumOfFuelToGetTo positions destination = sum $ map (\p -> abs (p - destination)) positions

solve1 :: [Int] -> Int
solve1 l = minimum $ map (sumOfFuelToGetTo l) [(minimum l)..(maximum l)]

sumOfFuelToGetTo2 :: [Int] -> Int -> Int
sumOfFuelToGetTo2 positions destination = sum $ map (\p -> let n = abs (p - destination) in div (n * (n + 1)) 2) positions

solve2 :: [Int] -> Int
solve2 l = minimum $ map (sumOfFuelToGetTo2 l) [(minimum l)..(maximum l)]

main :: IO ()
--main = interact $ show . solve1 . parseInput
main = interact $ show . solve2 . parseInput
