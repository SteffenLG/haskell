import Data.Char

antallsifre cs = length [c | c <- cs, isDigit c]

ab :: String -> Bool
ab [] = True
ab ('a':'b':xs) = ab xs
ab (_:'b':xs) = False

fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)

nf = error "Not found"

bs :: Ord a => a -> [(a, b)] -> b
bs x [] = nf
bs x [(y,z)] | x == y = z
             | otherwise = nf
bs x l = let m = length l `div` 2; (n, v) = (l !! m) in
    if n == x then 
        v
    else if n < x then
        bs x (drop m l)
    else 
        bs x (take m l)
