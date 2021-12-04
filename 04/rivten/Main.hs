module Main where

import Control.Applicative
import Data.List.Split
import Data.List

getInputData :: String -> ([Int], [[[Int]]])
getInputData s = let firstLine:remaining = splitOn "\n\n" s
                  in (map read $ splitOn "," firstLine, map ((map ((map read) . words)) . lines) remaining)

isVictoryTable :: [[Int]] -> Bool
isVictoryTable t = (any null t) || (any null $ transpose t)

-- NOTE: this logic is just plain wrong...
-- It just happened to work for my sample input.
-- You cannot remove element from a table otherwise it causes
-- mismatch in the transpose
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

isRealVictoryTable :: [[Int]] -> [Int] -> Bool
isRealVictoryTable table ns = (any (all (`elem` ns)) table) || (any (all (`elem` ns)) $ transpose table)

isInTable :: [[Int]] -> Int -> Bool
isInTable t e = any (elem e) t

getRealTableScore :: [[Int]] -> [Int] -> Int
getRealTableScore table es = ((sum $ map sum table) - (sum $ filter (isInTable table) es)) * (head es)

partTwo :: [Int] -> [[[Int]]] -> Int
partTwo numbers tables = aux numbers tables 0 []
    where aux [] _ lastVictoryScore _ = lastVictoryScore
          aux (n:ns) notYetWonTables lastVictoryScore listOfProcessedNumbers = 
              let newListOfProcessedNumbers = n:listOfProcessedNumbers
                  newVictoryTables = filter (\t -> isRealVictoryTable t newListOfProcessedNumbers) notYetWonTables
                  newNotYetWonTables = filter (\t -> not $ isRealVictoryTable t newListOfProcessedNumbers) notYetWonTables
               in if null newVictoryTables then aux ns newNotYetWonTables lastVictoryScore newListOfProcessedNumbers
                                           else let newLastVistoryScore = getRealTableScore (head newVictoryTables) newListOfProcessedNumbers
                                                 in aux ns newNotYetWonTables newLastVistoryScore newListOfProcessedNumbers


main :: IO ()
main = interact $ show . liftA2 (,) (uncurry partOne) (uncurry partTwo) . getInputData
