import Data.List (intersect)
import Util (readLines, splitOn, unique)

main :: IO ()
main = do
  input <- readLines "inputs/d6.txt"
  let sp = splitOn "" input
  let anyCounts = map any sp
  putStrLn $ "Part 1: " ++ show (sum anyCounts)
  let everyCounts = map every sp
  putStrLn $ "Part 2: " ++ show (sum everyCounts)
  where
    any :: [String] -> Int
    any = length . unique . concat

    every :: [String] -> Int
    every = length . foldl1 intersect