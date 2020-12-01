module Main where
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let input = map read (lines contents)
  putStrLn "Part 1:"
  print $ part1 input

part1 :: [Integer] -> Integer
part1 xs = head [x*y | (x, y) <- allPairs $ sort xs, x+y == 2020]

allPairs :: [Integer] -> [(Integer, Integer)]
allPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]
