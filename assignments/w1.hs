-- Answer to the Week 1 assignment

--B
--1.7.4
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <- xs, b > x]

qsortrev [] = []
qsortrev (x:xs) = qsort larger ++ [x] ++ qsort smaller
                where 
                    smaller = [a | a <- xs, a >= x]
                    larger = [b | b <- xs, b < x]

--1.7.5
--Duplicates are deleted

--2.7.4
lst xs = xs !! (length xs - 1)
lst2 [x] = x
lst2 (x:xs) = lst2 xs

--2.7.5
init1 xs = take ((length xs) - 1) xs
init2 xs = reverse (tail (reverse xs))

--C
-- Assignment example is wrong here. plu [1,2,3] 5 returns [6,7,8]. Not [5,6,7]
plu ks n = [e + n | e <- ks]

pali as = as == reverse as