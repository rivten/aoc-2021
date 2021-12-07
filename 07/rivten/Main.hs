module Main where

import Data.List.Split

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

sumOfFuelToGetTo :: [Int] -> Int -> Int
sumOfFuelToGetTo positions destination = sum $ map (\p -> abs (p - destination)) positions

solve1 :: [Int] -> Int
solve1 l = minimum $ map (sumOfFuelToGetTo l) [(minimum l)..(maximum l)]

main :: IO ()
main = interact $ show . solve1 . parseInput
