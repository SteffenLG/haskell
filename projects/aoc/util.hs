module Util where

import Data.Set (fromList, toList)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
import Text.Read

readLines :: FilePath -> IO [String]
readLines path = do
  contents <- readFile path
  return $ lines contents

readInts :: FilePath -> IO [Int]
readInts path = do
  lines <- readLines path
  return $ map read lines

timeNanos, timeMicros, timeMillis :: IO Integer

t mul = round . (mul *) <$> getPOSIXTime

timeNanos = t 1000000000

timeMicros = t 1000000

timeMillis = t 1000

getValue :: Read a => String -> IO a
getValue msg = do
  putStrLn msg
  l <- getLine
  let mb = readMaybe l
  case mb of
    Just a -> do return a
    Nothing -> do
      putStrLn "Invalid input, please try again"
      getValue msg

unique :: Ord a => [a] -> [a]
unique = toList . fromList

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn d xs = first : splitOn d rest
  where
    first = takeWhile (/= d) xs
    tmp = dropWhile (/= d) xs
    rest = if tmp /= [] then tail tmp else []
