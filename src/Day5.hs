module Main where
import Prelude hiding (Left, Right) -- Don't comment, I'm too lazy to make different names
import Data.List.Split (chunksOf)
import Data.Sort (sort)

data Side = Front | Back | Left | Right deriving Show

type BoardingPass = [Side]

newtype Range = Range (Int, Int) deriving Show

newtype Range2D = Range2D (Range, Range) deriving Show

class Partitionable a where
    (</>) :: a -> Side -> a

instance Read Side where
    readsPrec _ input =
        case input of
        ('F':rest) -> [(Front, rest)]
        ('B':rest) -> [(Back, rest)]
        ('L':rest) -> [(Left, rest)]
        ('R':rest) -> [(Right, rest)]
        _ -> []

instance Partitionable Range where
    (Range (from, to)) </> Front = Range (from, quot (from+to) 2)
    (Range (from, to)) </> Back = Range (quot (from+to) 2 + 1, to)
    (Range (from, to)) </> Left = Range (from, quot (from+to) 2)
    (Range (from, to)) </> Right = Range (quot (from+to) 2 + 1, to)

instance Partitionable Range2D where
    (Range2D (row, col)) </> Front = Range2D (row </> Front, col)
    (Range2D (row, col)) </> Back = Range2D (row </> Back, col)
    (Range2D (row, col)) </> Left = Range2D (row, col </> Left)
    (Range2D (row, col)) </> Right = Range2D (row, col </> Right)

main :: IO ()
main = do
  contents <- getContents
  let input = map (map read . chunksOf 1) $ lines contents :: [BoardingPass]
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

part1 :: [BoardingPass] -> Int
part1 xs = maximum $ map (seatId . getSeat . f) xs
    where f = foldl (</>) (Range2D (Range (0, 127), Range (0, 7)))

part2 :: [BoardingPass] -> Int
part2 xs = (+1) $ last $ head $ groupWhen (\a b -> b == (a+1)) $ sort ids
    where f = foldl (</>) (Range2D (Range (0, 127), Range (0, 7)))
          ids = map (seatId . getSeat . f) xs

getSeat :: Range2D -> (Int, Int)
getSeat (Range2D (Range (a, _), Range (b, _))) = (a, b) -- As optimistic as possible, I'm tired today

seatId :: (Int, Int) -> Int
seatId (a, b) = a*8  + b

-- Ripped from https://gitlab.haskell.org/ghc/ghc/-/issues/1408
groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ []    = []
groupWhen _ [a]   = [[a]]
groupWhen f (a:l) = if f a (head c) then (a:c):r else [a]:c:r
  where (c:r) = groupWhen f l

