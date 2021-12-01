module Main where

import System.IO
import Data.List.Split

increaseMap :: [Int] -> [Bool]
increaseMap xs = zipWith (<) xs (tail xs)

increaseSlidingMap :: [Int] -> [Bool]
increaseSlidingMap (w:rem@(_:_:z:_)) = (w < z):(increaseSlidingMap rem)
increaseSlidingMap _ = []

main :: IO ()
main = do
    content <- hGetContents stdin
    print $ length $ filter id $ increaseMap $ map read $ lines content
    print $ length $ filter id $ increaseSlidingMap $ map read $ lines content
