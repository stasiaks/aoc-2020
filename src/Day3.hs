module Main where
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector

data Field = Snow | Tree deriving (Show)

type Grid = Array V DIM2 Field

readField :: Char -> Field
readField '.' = Snow
readField '#' = Tree

main :: IO ()
main = do
  contents <- getContents
  let input = Prelude.map (Prelude.map readField) $ lines contents :: [[Field]]
      yDim = length input
      xDim = length $ head input
      grid = fromListVector (Z :. yDim :. xDim :: DIM2) $ concat input
  putStrLn "Part 1:"
  print $ part1 grid
  putStrLn "Part 2:"
  print $ part2 grid

part1 :: Grid -> Integer
part1 grid = countTreeHits grid (ix2 0 0) f
    where (Z :. sizeY :. sizeX) = extent grid
          f = nextCoordinatesForWidth sizeY sizeX 1 3

part2 :: Grid -> Integer
part2 grid = Prelude.product $ Prelude.map (countTreeHits grid (ix2 0 0)) [f1, f2, f3, f4, f5]
    where (Z :. sizeY :. sizeX) = extent grid
          f1 = nextCoordinatesForWidth sizeY sizeX 1 1
          f2 = nextCoordinatesForWidth sizeY sizeX 1 3
          f3 = nextCoordinatesForWidth sizeY sizeX 1 5
          f4 = nextCoordinatesForWidth sizeY sizeX 1 7
          f5 = nextCoordinatesForWidth sizeY sizeX 2 1

countTreeHitsT :: Grid -> DIM2 -> (DIM2 -> Maybe DIM2) -> Integer -> Integer
countTreeHitsT g c f a = case f c of
  Nothing -> a
  Just nextCoord -> case g ! nextCoord of
    Snow -> countTreeHitsT g nextCoord f a
    Tree -> countTreeHitsT g nextCoord f (a+1)

countTreeHits :: Grid -> DIM2 -> (DIM2 -> Maybe DIM2) -> Integer
countTreeHits g c f = countTreeHitsT g c f 0

nextCoordinatesForWidth :: Int -> Int -> Int -> Int -> DIM2 -> Maybe DIM2
nextCoordinatesForWidth maxY maxX addY addX (Z :. y :. x) = do
  let nextX = (x + addX) `mod` maxX
      nextY = y + addY
  if nextY >= maxY then Nothing else Just $ ix2 nextY nextX
