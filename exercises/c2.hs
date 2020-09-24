--2.7.2
{-
    (2^3)*4
    (2*3)+(4*5)
    2+(3*(4^5))
-}

--2.7.3
n = a `div` length xs
    where 
        a = 10
        xs = [1, 2, 3, 4, 5]

--2.7.4
lst xs = xs !! (length xs - 1)
lst2 [x] = x
lst2 (x:xs) = lst2 xs

--2.7.5
init1 xs = take ((length xs) - 1) xs
init2 xs = reverse (tail (reverse xs))