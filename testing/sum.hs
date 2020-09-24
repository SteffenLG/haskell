
getSum :: [Int] -> Int
getSum [] = 0
getSum [x] = x
getSum (x:xs) = x + (getSum xs)

succc :: Int -> Int
succc x = x + 1
