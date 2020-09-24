--6.8.1
factorial :: Int -> Int
factorial 0 = 1
factorial x | x < 0 = error "Invalid input"
            | otherwise = x * factorial (x - 1)

--6.8.2
sumdown :: Int -> Int
sumdown 1 = 1
sumdown x = x + sumdown (x - 1)

--6.8.3
ass :: Int -> Int -> Int
_ `ass` 0 = 1
0 `ass` _ = 0
x `ass` 1 = x
1 `ass` _ = 1
x `ass` n = x * x `ass` (n - 1) 

--6.8.4
euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x > y = euclid (x - y) y
           | otherwise = euclid x (y - x)

--6.8.6
--a
andd :: [Bool] -> Bool
andd [] = True
andd (x:xs) | x = andd xs
            | otherwise = x

--b
concatt :: [[a]] -> [a]
concatt [] = []
concatt (x:xs) = x ++ concatt xs

--c
replicatee :: Int -> a -> [a]
replicatee 0 _ = []
replicatee 1 x = [x]
replicatee n x = x : replicatee (n-1) x

--d 
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of bounds"
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n - 1)

--e
elemm :: Eq a => a -> [a] -> Bool
elemm _ [] = False
elemm x (y:ys) | x == y = True
               | otherwise = elemm x ys

--6.8.7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--6.8.8
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take half xs, drop half xs)
            where half = (length xs) `div` 2

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort f) (mergeSort l)
                where (f,l) = halve xs

--6.8.9
--a
summ :: Num a => [a] -> a
summ [] = 0
summ [x] = x
summ (x:xs) = x + sum xs

--b
takee :: Int -> [a] -> [a]
takee 0 _ = []
takee _ [] = error "List has too few elements"
takee n (x:xs) = x : takee (n - 1) xs

--c 
lastt :: [a] -> a
lastt [x] = x
lastt (x:xs) = lastt xs