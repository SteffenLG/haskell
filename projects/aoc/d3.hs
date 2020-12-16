import Util

data Vector = Vector Int Int deriving Show

slopes = [
    Vector 1 1,
    Vector 3 1,
    Vector 5 1,
    Vector 7 1,
    Vector 1 2
    ]

main :: IO ()
main = do
    input <- readLines "inputs/d3.txt"
    mapM_ (printSlope input) slopes
    putStrLn $ "Multiplied: " ++ (show $ foldl (*) 1 (map (\s -> hits s input) slopes))

printSlope :: [String] -> Vector -> IO ()
printSlope m (Vector x y) = 
    putStrLn $ "Number of trees hit for slope: " ++ (show x) ++ ", " ++ (show y) ++ ": " ++ (show $ hits (Vector x y) m)

hits :: Vector -> [String] -> Int
hits v m = length [t | t <- path v (Vector 0 0) m, t == '#']

path :: Vector -> Vector -> [String] -> [Char]
path (Vector dx dy) (Vector x y) m 
    | y >= (length m) = []
    | otherwise = (line !! (x `mod` (length line))) : rest
    where 
        line = m !! y
        rest = path (Vector dx dy) (Vector (x + dx) (y + dy)) m