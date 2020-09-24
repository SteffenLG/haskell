--1.7.1
{-
    There are no alternate calculations. You either start with the inner function or the outer
    function, those are the only two options and any variations on those are pointless semantics.
    Yes, the mathematical + operator is commutative so you can put parentheses wherever you want. 
    I don't see how that helps me learn Haskell. I could just copy the answer key from the book
    but I'm not going to.
-}

--1.7.2
{-
    sum [] = 0
    sum x = x + (sum []) = x + 0 = x
-}

-- 1.7.3
prod [] = 0
prod [x] = x
prod (first:rest) = first * (prod rest)

-- prod [2, 3, 4] returns 24

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