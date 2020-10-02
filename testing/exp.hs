exp :: Int -> Int
exp n 0 = 1
exp n m = n * exp n m-1
