module Main where

import Data.List
import Data.List.Split

data Segment = Segment { start :: (Int, Int)
                       , end :: (Int, Int)
                       } deriving (Show)

parseSegment :: String -> Segment
parseSegment s = let [startString, endString] = splitOn "->" s
                     [xS, yS] = splitOn "," startString
                     [xE, yE] = splitOn "," endString
                  in Segment { start = (read xS, read yS), end = (read xE, read yE) }

isStraightLine :: Segment -> Bool
isStraightLine (Segment { start = (xS, _), end = (xE, _) }) | xS == xE = True
isStraightLine (Segment { start = (_, yS), end = (_, yE) }) | yS == yE = True
isStraightLine _ = False

coveredPoints :: Segment -> [(Int, Int)]
coveredPoints (Segment { start = (xS, yS), end = (xE, yE) }) | xS == xE = [(xS, y) | y <- [(min yS yE)..(max yS yE)]]
coveredPoints (Segment { start = (xS, yS), end = (xE, yE) }) | yS == yE = [(x, yS) | x <- [(min xS xE)..(max xS xE)]]
coveredPoints _  = undefined

parseInput :: [String] -> [Segment]
parseInput = map parseSegment

countRepeatingElements :: Eq a => [a] -> Int
countRepeatingElements [] = 0
countRepeatingElements (a:as) = aux a False 0 as
    where aux :: Eq a => a -> Bool -> Int -> [a] -> Int
          aux _ _ count [] = count
          aux e True count (x:xs) | e == x = aux e True count xs
          aux e False count (x:xs) | e == x = aux e True (count + 1) xs
          aux _ _ count (x:xs) = aux x False count xs

main :: IO ()
main = interact $ show . countRepeatingElements . sort . concat . map coveredPoints . filter isStraightLine . parseInput . lines
