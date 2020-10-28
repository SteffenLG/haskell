import Data.List
-- 1
-- a
al1 :: [Bool] -> Bool
al1 [] = True
al1 (True:xs) = al1 xs
al1 _ = False

-- b
al2 :: [Bool] -> Bool
al2 = and

-- c
al3 :: [Bool] -> Bool
al3 = foldl (&&) True

-- d
al4 :: [Bool] -> Bool
al4 = foldr (&&) True

-- 2
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala = foldl

-- 3
trekant :: Int -> IO ()
trekant n = putStr $ unlines $ take n $ iterate ("* "++) "*"

-- 4
juletre :: Int -> IO ()
juletre n = putStr $ unlines $ map print $ zip [n-1, n-2..0] [1..n] 
    where
        print = \(sp, st) -> (replicate sp ' ') ++ (concat $ (replicate st "* "))