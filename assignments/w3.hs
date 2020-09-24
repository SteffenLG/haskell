--A
--4.8.5
(&&&) :: Bool -> Bool -> Bool
x &&& y = if x then 
                (if y then 
                    True 
                else 
                    False
                )
            else 
                False

--4.8.7
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

--B
--5.7.6
factors :: Int -> [Int]
factors x = [f | f <- [1..x-1], x `mod` f == 0]

perfects :: Int -> [Int]
perfects x = [p | p <- [1..x], sum (factors p) == p]

--5.7.7 
f :: [(Int, Int)]
f = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

--5.7.9
scalarproduct :: Num a => [a] -> [a] -> a
scalarproduct xs ys = sum [(xs !! i) * (ys !! i) | i <- [0..(length xs)-1]]

--C
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] x = []
rem1 (x:xs) y = if x == y then
                    xs
                else  
                    x : rem1 xs y

--D
diff :: Eq a => [a] -> [a] -> [a]
diff as [] = as
diff [] bs = []
diff as (b:bs) = diff (rem1 as b) bs