module Main where

import System.IO
import Data.List.Split

getList :: [Char] -> [Int]
getList content = map read $ filter ((/= 0) . length) $ splitOn "\n" content

increaseMap :: [Int] -> [Bool]
increaseMap [] = []
increaseMap [_] = []
increaseMap (x:y:xs) = (if x < y then True else False):(increaseMap (y:xs))

main :: IO ()
main = do
    content <- hGetContents stdin
    print $ length $ filter id $ increaseMap $ getList content
