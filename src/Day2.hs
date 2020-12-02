module Main where
import Data.Char (isDigit, isSpace)
import Data.List.Split (splitOn)


trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Policy = Policy
    { minOccurences :: Int
    , maxOccurences :: Int
    , char :: Char } deriving (Show)

data PasswordEntry = PasswordEntry
    { policy :: Policy
    , password :: String } deriving (Show)

instance Read Policy where
    readsPrec _ input =
        let (minString, rest1) = span isDigit input
            minValue = read minString :: Int
            (_:rest2) = rest1
            (maxString, rest3) = span isDigit rest2
            maxValue = read maxString :: Int
            (_:char:rest4) = rest3
            in [(Policy {minOccurences=minValue, maxOccurences=maxValue, char=char}, rest4)]

instance Read PasswordEntry where
    readsPrec _ input =
        let [policyString, rest] = splitOn ":" input
            policy = read policyString :: Policy
            password = trim rest
            in [(PasswordEntry {policy=policy, password=password}, "")]

main :: IO ()
main = do
  contents <- getContents
  let input = map read (lines contents) :: [PasswordEntry]
  putStrLn "Part 1:"
  print $ part1 input
  --putStrLn "Part 2:"
  --print $ part2 input

part1 :: [PasswordEntry] -> Int
part1 xs = length $ filter validate xs

validate :: PasswordEntry -> Bool
validate PasswordEntry{policy = a, password = b} = runPolicy a b

runPolicy :: Policy -> String -> Bool
runPolicy policy password = 
    let occurences = length $ filter (==char policy) password
        in minOccurences policy <= occurences && maxOccurences policy >= occurences

