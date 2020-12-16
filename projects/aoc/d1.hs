import Util
import Data.List

data Pair = Pair Int Int deriving (Show)

data Triple = Triple Int Int Int deriving (Show)

main :: IO ()
main = do
    input <- readInts "inputs/d1.txt"
    let dSum = maybe 0 (\(Pair a b) -> a * b) $ findPairSum 2020 input
    let tSum = maybe 0 (\(Triple a b c) -> a * b * c) $ findTripleSum 2020 input
    let message = "Double sum: " ++ (show dSum) ++ "\nTriple sum: " ++ (show tSum) ++ "\n"
    putStr message
    
findPairSum :: Int -> [Int] -> Maybe Pair
findPairSum x xs = 
    case x `elemIndex` pSums of
        Just n -> Just(p !! n)
        Nothing -> Nothing
    where
        p = pairs xs
        pSums = map (\(Pair a b) -> a + b) p

findTripleSum :: Int -> [Int] -> Maybe Triple
findTripleSum x xs = 
    case x `elemIndex` tSums of
        Just n -> Just(t !! n)
        Nothing -> Nothing
    where
        t = triples xs
        tSums = map (\(Triple a b c) -> a + b + c) t

triples :: [Int] -> [Triple]
triples [] = []
triples (x:xs) = [Triple x y z | Pair y z <- pairs xs] ++ triples xs

pairs :: [Int] -> [Pair]
pairs [] = []
pairs (x:xs) = [Pair x y | y <- xs] ++ pairs xs