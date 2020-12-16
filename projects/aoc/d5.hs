import Data.List (sort)
import Util (readLines)

data Seat = Seat Int Int deriving (Show)

main :: IO ()
main = do
  input <- readLines "inputs/d5.txt"
  let seats = map seat input
  let seatIds = map seatId seats
  let maxId = maximum seatIds
  print maxId
  let mySeat = findMissing $ sort seatIds
  print mySeat

seat :: String -> Seat
seat xs = Seat r c
  where
    r = binPath 0 127 $ takeWhile fOrB xs
    c = binPath 0 7 $ dropWhile fOrB xs
    fOrB = \c -> c == 'F' || c == 'B'

binPath :: Int -> Int -> String -> Int
binPath mi ma [] =
  if mi == ma
    then mi
    else error ("Inconclusive search max=" ++ show ma ++ " min=" ++ show mi)
binPath mi ma (x : xs)
  | x `elem` ['F', 'L'] = binPath mi mx xs
  | x `elem` ['B', 'R'] = binPath my ma xs
  | otherwise = binPath mi ma xs
  where
    mx = mi + (ma - mi) `div` 2
    my = mi + ((ma - mi) `div` 2) + 1

seatId :: Seat -> Int
seatId (Seat r c) = r * 8 + c

findMissing :: [Int] -> Int
findMissing [_] = -1
findMissing (x : y : xs) =
  if y == x + 2
    then x + 1
    else findMissing (y : xs)