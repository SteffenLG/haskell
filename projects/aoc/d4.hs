import Data.List (elemIndex, intersperse, isInfixOf)
import Data.Maybe ()
import Util (readLines)

newtype Passport = Passport [Property] deriving (Show)

data Property = Property String String deriving (Show)

main :: IO ()
main = do
  input <- readLines "inputs/d4.txt"
  let formatted = format input
  let passports = parseAll formatted
  let validPassports = filter valid passports
  print $ length validPassports

format :: [String] -> [String]
format [] = []
format ([] : xs) = format xs
format xs = unwords (intersperse " " current) : format rest
  where
    current = takeWhile notEmpty xs
    rest = dropWhile notEmpty xs
    notEmpty = (/= "")

parseAll :: [String] -> [Passport]
parseAll = map (parse . words)

parse :: [String] -> Passport
parse [] = Passport []
parse (x : xs) = Passport (Property n v : r)
  where
    (n, v) = split ':' x
    Passport r = parse xs

split :: Char -> String -> (String, String)
split d xs = case elemIndex d xs of
  Just i -> (take i xs, drop (i + 1) xs)
  Nothing -> (xs, "")

valid :: Passport -> Bool
valid p = all (`elem` props p) required

props :: Passport -> [String]
props (Passport []) = []
props (Passport (p : px)) = n : props (Passport px)
  where
    Property n _ = p

required :: [String]
required = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

{-
byr (Birth Year) - four digits; at least 1920 and at most 2002.
iyr (Issue Year) - four digits; at least 2010 and at most 2020.
eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
hgt (Height) - a number followed by either cm or in:
If cm, the number must be at least 150 and at most 193.
If in, the number must be at least 59 and at most 76.
hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
pid (Passport ID) - a nine-digit number, including leading zeroes.
cid (Country ID) - ignored, missing or not.
-}
