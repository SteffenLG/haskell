--4.8.1
halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve xs = (take n xs, drop n xs)
            where n = length xs `div` 2

--4.8.2
--a
thirda :: [a] -> a
thirda xs = head (tail (tail xs))

--b
thirdb :: [a] -> a
thirdb xs = xs !! 2

--c
thirdc :: [a] -> a
thirdc (_:_:x:_) = x

--4.8.3
--a
safetaila :: [a] -> [a]
safetaila xs = if null xs then [] else tail xs

--b
safetailb :: [a] -> [a]
safetailb xs | null xs = []
             | otherwise = tail xs

--c
safetailc :: [a] -> [a]
safetailc [] = []
safetailc (x:xs) = xs

--4.8.4
(|||) :: Bool -> Bool -> Bool
(|||) True _ = True
(|||) _ True = True
(|||) _ _ = False

(||||) :: Bool -> Bool -> Bool
True |||| _ = True
_ |||| True = True
_ |||| _ = False

(|||||) :: Bool -> Bool -> Bool
False ||||| False = False
_ ||||| _ = True


(||||||) :: Bool -> Bool -> Bool
x |||||| y | x /= y = True 
           | x = x
           | otherwise = False

--4.8.5
(&&&) :: Bool -> Bool -> Bool
x &&& y = if x then 
                (if y then 
                    True 
                else 
                    False
                )
            else 
                False

--4.8.6
(&&&&) :: Bool -> Bool -> Bool
x &&&& y = if x then y else x

--4.8.7
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult2 :: Int -> Int -> Int -> Int
mult2 = \x -> \y -> \z -> x * y * z

--4.8.8
luhnDouble :: Int -> Int
luhnDouble x = let x2 = x * 2 in if x2 > 9 then x2 - 9 else x2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

