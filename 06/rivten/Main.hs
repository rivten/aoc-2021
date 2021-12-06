module Main where

import Data.List.Split
import qualified Data.Map as Map

parseInput :: String -> [Integer]
parseInput = map read . splitOn ","


countElem :: Eq a => [a] -> [(a, Integer)]
countElem [] = []
countElem (n:ns) = let nsWithoutN = filter (/= n) ns
                       otherN = filter (== n) ns
                       nCount = 1 + (toInteger $ length otherN)
                    in (n, nCount):(countElem nsWithoutN)

makeInputAsMap :: [Integer] -> Map.Map Integer Integer
makeInputAsMap = Map.fromList . countElem

reborn :: Maybe Integer -> Map.Map Integer Integer -> Map.Map Integer Integer
reborn Nothing x = x
reborn (Just n) m = Map.insert 8 n $ Map.insert 6 (maybe n (+ n) (Map.lookup 6 m)) m

doOneSmartSim :: Map.Map Integer Integer -> Map.Map Integer Integer
doOneSmartSim sim = let nextMap = Map.mapKeys (\x -> x - 1) sim
                        maybeReborn = Map.lookup (-1) nextMap
                     in reborn maybeReborn $ Map.delete (-1) nextMap

doOneSim :: [Int] -> [Int]
doOneSim [] = []
doOneSim (0:ns) = 6:8:(doOneSim ns)
doOneSim (n:ns) = (n-1):(doOneSim ns)

main :: IO ()
--main = interact $ show . length . (!! 80) . iterate doOneSim . parseInput
main = interact $ show . sum . Map.elems . (!! 256) . iterate doOneSmartSim . makeInputAsMap . parseInput
