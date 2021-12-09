module Main where

import Data.List.Split

parseInputPart1 :: [String] -> [String]
parseInputPart1 = concat . (map (words . (!! 1) . (splitOn "|")))

isSimpleDigit :: String -> Bool
isSimpleDigit s = elem (length s) [2, 3, 4, 7]

main :: IO ()
main = interact $ show . length . filter isSimpleDigit . parseInputPart1 . lines
