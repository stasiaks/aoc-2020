module Main where
import Data.List (groupBy, nub)

type Answers = [Char]
type Group = [Answers]

main :: IO ()
main = do
  contents <- getContents
  let input = paragraphs $ lines contents :: [Group]
  putStrLn "Part 1:"
  print $ part1 input

part1 :: [Group] -> Int
part1 xs = sum $ map (length . nub . concat) xs

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)
