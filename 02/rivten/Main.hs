module Main where

import System.IO

acc :: (Int, Int) -> ([Char], Int) -> (Int, Int)
acc (x, y) ("forward", a) = (x + a, y)
acc (x, y) ("down", a) = (x, y + a)
acc (x, y) ("up", a) = (x, y - a)
acc _ _ = undefined

acc2 :: (Int, Int, Int) -> ([Char], Int) -> (Int, Int, Int)
acc2 (x, y, aim) ("forward", a) = (x + a, y + aim * a, aim)
acc2 (x, y, aim) ("down", a) = (x, y, aim + a)
acc2 (x, y, aim) ("up", a) = (x, y, aim - a)
acc2 _ _ = undefined

main :: IO ()
main = do
    content <- hGetContents stdin
    print $ (\(x, y) -> x * y) $ foldl acc (0,0) $ map ((\[a,b] -> (a, read b)) . words) $ lines content
    print $ (\(x, y, _) -> x * y) $ foldl acc2 (0,0, 0) $ map ((\[a,b] -> (a, read b)) . words) $ lines content
