module Main where
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

mergeParagraphs :: [[String]] -> [String]
mergeParagraphs = map unwords

paragraphs :: [String] -> [[String]]
paragraphs = map (filter $ not . null) . groupBy (const $ not . null)

part1 :: [Passport] -> Int
part1 xs = length $ filter validate xs

validate :: Passport -> Bool
validate xs = BirthYear `elem` ts
           && IssueYear `elem` ts
           && ExpirationYear `elem` ts
           && Height `elem` ts
           && HairColor `elem` ts
           && EyeColor `elem` ts
           && PassportId `elem` ts
    where ts = map (\(Field t _) -> t) xs
