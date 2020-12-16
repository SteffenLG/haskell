import Util

newtype Color = String
data Bag = Bag Color [(Int, Bag)]

main :: IO ()
main = do
    input <- readLines "inputs/d7.txt"
    let parsed = parseInput input

parseBags :: ([Bag], [[String]]) -> ([Bag], [[String]])
parseBags (bs, []) = (bs, [])
parseBags (bs, (x:xs)) = parseBags (b:bs, xs)
    where
        b = Bag c sb
        c = x !! 0
        sb = 

parseInput :: [String] -> (String, [(Int, String)])
parseInput (x:xs) = 