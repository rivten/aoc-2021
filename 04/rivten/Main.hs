module Main where

import Data.List.Split
import Data.List

getInputData :: String -> ([Int], [[[Int]]])
getInputData s = let firstLine:remaining = splitOn "\n\n" s
                  in (map read $ splitOn "," firstLine, map ((map ((map read) . words)) . lines) remaining)

isVictoryTable :: [[Int]] -> Bool
isVictoryTable t = (any null t) || (any null $ transpose t)

removeElemFromTable :: Int -> [[Int]] -> [[Int]]
removeElemFromTable e = map (filter (/= e))

partOne :: [Int] -> [[[Int]]] -> Int
partOne = aux undefined
    where aux :: Int -> [Int] -> [[[Int]]] -> Int
          aux _ [] _ = undefined
          aux previous (n:ns) bingoTables = 
              let victoryTables = filter isVictoryTable bingoTables
               in if not $ null victoryTables then (sum $ map sum $ head victoryTables) * previous
                                              else aux n ns (map (removeElemFromTable n) bingoTables)

main :: IO ()
main = interact $ show . (uncurry partOne) . getInputData
