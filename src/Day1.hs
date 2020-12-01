module Main where
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let input = map read (lines contents)
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

part1 :: [Integer] -> Integer
part1 xs = head [x*y | (x, y) <- allPairs $ sort xs, x+y == 2020]

part2 :: [Integer] -> Integer
part2 xs = head [x*y*z | (x, y, z) <- allTrios xs, x+y+z == 2020]

allPairs :: [Integer] -> [(Integer, Integer)]
allPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

allTrios :: [Integer] -> [(Integer, Integer, Integer)]
allTrios xs = [(x,y,z) | (x:ys) <- tails xs, (y, z) <- allPairs ys]
