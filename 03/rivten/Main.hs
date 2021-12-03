module Main where

import Data.List

gammaBinary :: [String] -> [Bool]
gammaBinary = map ((\l -> 2 * sum l > length l) . map (read . \x -> [x])) . transpose

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\acc b -> 2 * acc + if b then 1 else 0) 0

part1 :: [String] -> Int
part1 = (\l -> binaryToInt l * (binaryToInt $ map not l)) . gammaBinary

acc2A :: [String] -> Int -> [String]
acc2A [] _ = []
acc2A [x] _ = [x]
acc2A l n = let s = (transpose l) !! n
                oneMoreCommon = ((\ls -> 2 * sum ls >= length ls) . map (read . \x -> [x])) s
            in if oneMoreCommon then
                                filter (\ss -> ss !! n == '1') l
                                else
                                filter (\ss -> ss !! n == '0') l

acc2B :: [String] -> Int -> [String]
acc2B [] _ = []
acc2B [x] _ = [x]
acc2B l n = let s = (transpose l) !! n
                oneMoreCommon = ((\ls -> 2 * sum ls >= length ls) . map (read . \x -> [x])) s
            in if oneMoreCommon then
                                filter (\ss -> ss !! n == '0') l
                                else
                                filter (\ss -> ss !! n == '1') l

part2 :: [String] -> Int
part2 l = (binaryToInt $ fmap (== '1') $ aux1 l 0) * (binaryToInt $ fmap (== '1') $ aux2 l 0)
    where aux1 [x] _ = x
          aux1 ls n = let l' = acc2A ls n in aux1 l' (n + 1)
          aux2 [x] _ = x
          aux2 ls n = let l' = acc2B ls n in aux2 l' (n + 1)

main :: IO ()
main = interact $ unlines . fmap show . (\x -> map ($ x) [part1, part2]) . lines
