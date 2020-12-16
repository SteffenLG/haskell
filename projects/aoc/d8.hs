import Util

main :: IO ()
main = do
    lines <- readLines "inputs/d8.txt"
    let wLines = map words lines

