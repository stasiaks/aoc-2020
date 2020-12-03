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

part1 :: Grid -> Integer
part1 grid = countTreeHits grid (ix2 0 0) f
    where (Z :. sizeY :. sizeX) = extent grid
          f = nextCoordinatesForWidth sizeY sizeX 1 3

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
