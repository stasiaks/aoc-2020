module Main where
import Data.Char (isDigit)
import Data.List (groupBy)
import Data.List.Split (splitOn)

data FieldType = BirthYear
               | IssueYear
               | ExpirationYear
               | Height
               | HairColor
               | EyeColor
               | PassportId
               | CountryId
               deriving Eq

data Field = Field FieldType String

type Passport = [Field]

instance Read FieldType where
    readsPrec _ input = case splitAt 3 input of
        ("byr", rest) -> [(BirthYear, rest)]
        ("iyr", rest) -> [(IssueYear, rest)]
        ("eyr", rest) -> [(ExpirationYear, rest)]
        ("hgt", rest) -> [(Height, rest)]
        ("hcl", rest) -> [(HairColor, rest)]
        ("ecl", rest) -> [(EyeColor, rest)]
        ("pid", rest) -> [(PassportId, rest)]
        ("cid", rest) -> [(CountryId, rest)]

instance Read Field where
    readsPrec _ input =
        let [key, value] = splitOn ":" input
            in [(Field (read key) value, "")]

main :: IO ()
main = do
  contents <- getContents
  let input = map (map read) $  map (splitOn " ") $ mergeParagraphs $ paragraphs $ lines contents :: [Passport]
  putStrLn "Part 1:"
  print $ part1 input
  putStrLn "Part 2:"
  print $ part2 input

mergeParagraphs :: [[String]] -> [String]
mergeParagraphs = map unwords

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

part1 :: [Passport] -> Int
part1 xs = length $ filter validate xs

part2 :: [Passport] -> Int
part2 xs = length $ filter (all validateField) $ filter validate xs

validate :: Passport -> Bool
validate xs = BirthYear `elem` ts
           && IssueYear `elem` ts
           && ExpirationYear `elem` ts
           && Height `elem` ts
           && HairColor `elem` ts
           && EyeColor `elem` ts
           && PassportId `elem` ts
    where ts = map (\(Field t _) -> t) xs

validateField :: Field -> Bool
validateField (Field BirthYear a) = year >= 1920 && year <= 2002
    where year = read a :: Int
validateField (Field IssueYear a) = year >= 2010 && year <= 2020
    where year = read a :: Int
validateField (Field ExpirationYear a) = year >= 2020 && year <= 2030
    where year = read a :: Int
validateField (Field Height a) = case (v, unit) of
        (Nothing, _) -> False
        (Just h, "in") -> h >= 59 && h <= 76
        (Just h, "cm") -> h >= 150 && h <= 193
        _ -> False
    where (b, unit) = span isDigit a
          v = if all isDigit b then Just $ read b else Nothing :: Maybe Int
validateField (Field HairColor ('#':xs)) = all (`elem` "0123456789abcdef") xs
validateField (Field HairColor _) = False
validateField (Field EyeColor "amb") = True
validateField (Field EyeColor "blu") = True
validateField (Field EyeColor "brn") = True
validateField (Field EyeColor "gry") = True
validateField (Field EyeColor "grn") = True
validateField (Field EyeColor "hzl") = True
validateField (Field EyeColor "oth") = True
validateField (Field EyeColor _) = False
validateField (Field PassportId xs) = all isDigit xs && length xs == 9
validateField (Field CountryId _) = True
