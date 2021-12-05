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
coveredPoints (Segment { start = (xS, yS), end = (xE, yE) }) 
  | abs (xE - xS) == abs (yE - yS)
  = [(xS + signum (xE - xS) * delta, yS + signum (yE - yS) * delta) | delta <- [0..(abs (xE - xS))]]
coveredPoints _ = undefined

parseInput :: [String] -> [Segment]
parseInput = map parseSegment

countRepeatingElements :: Eq a => [a] -> Int
countRepeatingElements = (\(s, _, _) -> s) . foldr aux (0, Nothing, False)
    where aux :: Eq a => a -> (Int, Maybe a, Bool) -> (Int, Maybe a, Bool)
          aux e (score, Nothing, _) = (score, Just e, False)
          aux e (score, Just n, True) | e == n = (score, Just n, True)
          aux e (score, Just n, False) | e == n = (score + 1, Just n, True)
          aux e (score, _, _) = (score, Just e, False)

main :: IO ()
--main = interact $ show . countRepeatingElements . sort . concat . map coveredPoints . filter isStraightLine . parseInput . lines
main = interact $ show . countRepeatingElements . sort . concat . map coveredPoints . parseInput . lines
