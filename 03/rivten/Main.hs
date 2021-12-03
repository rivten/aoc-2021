module Main where

import Data.List

gammaBinary :: [String] -> [Bool]
gammaBinary = map ((\l -> 2 * sum l > length l) . map (read . \x -> [x])) . transpose

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\acc b -> 2 * acc + if b then 1 else 0) 0

main :: IO ()
main = interact $ show . (\l -> binaryToInt l * (binaryToInt $ map not l)) . gammaBinary . lines
