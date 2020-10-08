-- A
-- 7.9.1
fm :: (a -> a) -> (a -> Bool) -> [a] -> [a]
fm f p = map f . filter p

-- 7.9.4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

-- 7.9.5
currry :: ((a, b) -> c) -> a -> b -> c
currry f = \x y -> f (x, y)

uncurrry :: (a -> b -> c) -> (a, b) -> c
uncurrry f = \(x, y) -> f x y

-- 7.9.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = zipWith ($) $ cycle [f, g]

-- 8.9.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y) 

-- 8.9.6
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (\x -> 1) (+)

-- B
infiks :: Expr -> String
infiks = folde show (\x y -> concat ["(", x, " + ", y, ")"])

prefiks :: Expr -> String
prefiks = folde show (\x y -> concat ["+ ", x, " ", y])

postfiks :: Expr -> String
postfiks = folde show (\x y -> concat [x, " ", y, " ", "+"])
