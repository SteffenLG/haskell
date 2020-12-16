import Data.Char
import Util

data Policy = Policy Int Int Char deriving Show
newtype Password = Password String deriving Show
data Entry = Entry Policy Password deriving Show

main :: IO ()
main = do
    input <- readLines "inputs/d2.txt"
    let entries = parse input
    let validEntries = filter valid entries
    putStrLn $ "Number of valid passwords: " ++ (show $ length validEntries)
    let udValidEntries = filter udValid entries
    putStrLn $ "Number of valid passwords with updated policy: " ++ (show $ length udValidEntries)

udValid :: Entry -> Bool
udValid (Entry p pw) = followsUdPolicy p pw

followsUdPolicy :: Policy -> Password -> Bool
followsUdPolicy (Policy mi ma c) (Password pw) = (indexMatches mi c pw) `xor` (indexMatches ma c pw)

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (b && not a)

indexMatches :: (Eq a, Ord a) => Int -> a -> [a] -> Bool
indexMatches i c xs
    | i' >= (length xs) = False
    | (xs !! i') == c = True
    | otherwise = False
    where
        i' = i - 1

valid :: Entry -> Bool
valid (Entry p pw) = followsPolicy p pw

followsPolicy :: Policy -> Password -> Bool
followsPolicy (Policy mi ma c) (Password pw) = (mi <= l) && (l <= ma)   
    where
        l = length $ filter (==c) pw

parse :: [String] -> [Entry]
parse = map parseEntry

parseEntry :: String -> Entry
parseEntry xs = Entry p pw
    where
        (p, xs') = parsePolicy xs
        pw = Password xs'

parsePolicy :: String -> (Policy, String)
parsePolicy xs = (p, r)
    where
        (mi, xs') = parseInt xs
        (ma, xs'') = parseInt xs'
        xs''' = dropWhile (not . isAlpha) xs''
        (c, xs'''') = (head xs''', tail xs''')
        p = Policy mi ma c
        r = dropWhile (not . isAlpha) xs''''

parseInt :: String -> (Int, String)
parseInt xs = (n, xs')
    where
        n = read $ takeWhile isDigit xs
        xs' = tail $ dropWhile isDigit xs