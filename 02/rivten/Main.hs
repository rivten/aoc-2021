module Main where

import System.IO

acc :: (Int, Int) -> ([Char], Int) -> (Int, Int)
acc (x, y) ("forward", a) = (x + a, y)
acc (x, y) ("down", a) = (x, y + a)
acc (x, y) ("up", a) = (x, y - a)
acc _ _ = undefined

main :: IO ()
main = do
    content <- hGetContents stdin
    print $ (\(x, y) -> x * y) $ foldl acc (0,0) $ map ((\[a,b] -> (a, read b)) . words) $ lines content
