--A - Not compulsory

--B
--3.11.3
second :: [a] -> a
second xs = head (tail xs)   

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)    

pair :: a -> b -> (a,b)
pair x y = (x,y)  

double :: Num a => a -> a
double x = x * 2                    

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs    

twice :: (a -> a) -> a -> a
twice f x = f (f x)                 

--C
{-
    False :: Bool
    5 + 8 :: Num a => a
    (+) 2 :: Num a => a -> a
    (+2)  :: Num a => a -> a
    (2+)  :: Num a => a -> a
    (["foo", "bar"], 'a') :: ([[Char]], Char)
    [(True, []), (False, [['a']])] :: [(Bool, [[Char]])]
    \x y -> y !! x :: Int -> [a] -> a
    [take, drop, \x y -> (y !! x)] :: Error
    [take, drop, \x y -> [y !! x]] :: [Int -> [a] -> [a]]
-}

--D
foo1 :: a -> b -> (a,b)
foo1 x y = (x, y) 

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y) -- Eq foo1

foo3 :: a -> b -> (a,b)
foo3 = \x y -> (x, y) -- Eq foo1

foo4 :: a -> b -> (a,b)
foo4 = \x -> \y -> (x, y) -- Eq foo1

foo5 :: a -> b -> (b,a)
foo5 = \x -> \y -> (y,x)

foo6 :: a -> b -> (a,b)
foo6 = \y -> \x -> (y,x) -- Eq foo1

-- All functions are equivalent, except foo5. 

--E
f1 :: a -> (a,a)
f1 x = (x,x)

f2 :: (a,b) -> a
f2 (x,y) = x

f3 :: (a,b) -> b
f3 (x,y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

--F
f :: Int -> Int -> Int
f x y = x + y

g :: (Int, Int) -> Int
g (x,y) = f x y