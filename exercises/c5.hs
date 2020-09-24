--5.7.1
sumOfSquares :: Int
sumOfSquares = sum [x * x | x <- [1..100]]

--5.7.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

--5.7.3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- (grid n n), x /= y]

--5.7.4
doReplicate :: Int -> a -> [a]
doReplicate x e | x < 1 = []
                | otherwise = [e | _ <- [1..x]] 

--5.7.5
pyths :: Int -> [(Int, Int, Int)]
pyths x = [(x,y,z) | x <- n, y <- n, z <- n, x*x + y*y == z*z]
        where n = [1..x]

--5.7.6
factors :: Int -> [Int]
factors x = [f | f <- [1..x-1], x `mod` f == 0]

perfects :: Int -> [Int]
perfects x = [p | p <- [1..x], sum (factors p) == p]

--5.7.7 
f = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

--5.7.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions k xs = find k [x | x <- (zip xs [0..])]

--5.7.9
scalarproduct xs ys = sum [(xs !! i) * (ys !! i) | i <- [0..(length xs)-1]]
