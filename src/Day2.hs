module Main where
import Data.Char (isDigit, isSpace)
import Data.List.Split (splitOn)
import Data.Algebra.Boolean (xor)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

data Policy = Policy
    { firstNumber :: Int
    , secondNumber :: Int
    , char :: Char } deriving (Show)

data PasswordEntry = PasswordEntry
    { policy :: Policy
    , password :: String } deriving (Show)

instance Read Policy where
    readsPrec _ input =
        let (fstString, rest1) = span isDigit input
            fstValue = read fstString :: Int
            (_:rest2) = rest1
            (sndString, rest3) = span isDigit rest2
            sndValue = read sndString :: Int
            (_:char:rest4) = rest3
            in [(Policy {firstNumber=fstValue, secondNumber=sndValue, char=char}, rest4)]

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
  putStrLn "Part 2:"
  print $ part2 input

part1 :: [PasswordEntry] -> Int
part1 xs = length $ filter (validate runPolicy1) xs

part2 :: [PasswordEntry] -> Int
part2 xs = length $ filter (validate runPolicy2) xs

validate :: (Policy -> String -> Bool) -> PasswordEntry -> Bool
validate rule PasswordEntry{policy = a, password = b} = rule a b

runPolicy1 :: Policy -> String -> Bool
runPolicy1 policy password = 
    let occurences = length $ filter (==char policy) password
        in firstNumber policy <= occurences && secondNumber policy >= occurences

runPolicy2 :: Policy -> String -> Bool
runPolicy2 Policy{firstNumber = a, secondNumber = b, char = c} password =
    password!!(a-1) == c `xor` password!!(b-1) == c

