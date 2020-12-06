module Main where
import Data.List (groupBy, nub, intersect)

type Answers = [Char]
type Group = [Answers]

main :: IO ()
main = do
  contents <- getContents
  let input = paragraphs $ lines contents :: [Group]
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

part1 :: [Group] -> Int
part1 xs = sum $ map (length . nub . concat) xs

part2 :: [Group] -> Int
part2 xs = sum $ map (length . foldl1 intersect) xs

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

