module Main where

import Data.List.Split

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

doOneSim :: [Int] -> [Int]
doOneSim [] = []
doOneSim (0:ns) = 6:8:(doOneSim ns)
doOneSim (n:ns) = (n-1):(doOneSim ns)

main :: IO ()
main = interact $ show . length . (\l -> iterate doOneSim l !! 80) . parseInput
