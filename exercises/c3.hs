--3.11.1
{-
    What are the types of the following values?
    ['a','b','c'] :: [Char]
    ('a','b','c') :: (Char, Char, Char)
    [(False, '0'), (True, '1')] :: [(Bool, Char)]
    ([False, True], ['0','1']) :: ([Bool], [Char])
    [tail, init, reverse] :: [[a] -> [a]]
-}

--3.11.2
bools :: [Bool]
bools = [True]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy a = (a,a)

apply :: (a -> b) -> a -> b
apply f x = f x

--3.11.3
second xs = head (tail xs)          -- [a] -> a
swap (x,y) = (y,x)                  -- (a,b) -> (b,a)
pair x y = (x,y)                    -- a -> b -> (a,b)
double x = x * 2                    -- Num a => a -> a
palindrome xs = reverse xs == xs    -- Eq a => [a] -> Bool
twice f x = f (f x)                 -- (a -> a) -> a -> a

--3.11.4
--Done

--3.11.5
{-
    It is not feasible because proving that two functions are equivalent is not a trivial task.
    If the domain of the function is very limited, such as for a Bool -> Bool function, it would be fairly
    trivial to check for equality, but for large domains it is infeasible.
-}