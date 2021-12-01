module Main where

import System.IO
import Data.List.Split

getList :: [Char] -> [Int]
getList content = map read $ filter ((/= 0) . length) $ splitOn "\n" content

increaseMap :: [Int] -> [Bool]
increaseMap xs = zipWith (\x y -> x < y) xs (tail xs)

main :: IO ()
main = do
    content <- hGetContents stdin
    print $ length $ filter id $ increaseMap $ getList content
