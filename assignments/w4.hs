--A
--(1)
fjernRec :: String -> Char -> String
fjernRec [] _ = []
fjernRec (x:xs) y | x == y = fjernRec xs y
                  | otherwise = x : fjernRec xs y
--(2)
fjern :: String -> Char -> String
fjern xs y = [x | x <- xs, x /= y]

--B
--(1)
tegnposRec :: Char -> String -> [Int]
tegnposRec _ [] = []
tegnposRec x ys = posHelperRec x (zip ys [0..])

posHelperRec :: Char -> [(Char, Int)] -> [Int]
posHelperRec _ [] = []
posHelperRec x ((y, i):ys) | x == y = i : posHelperRec x ys
                           | otherwise = posHelperRec x ys
--(2)
tegnpos :: Char -> String -> [Int]
tegnpos x ys = [i | (c, i) <- zip ys [0..], x == c]

--C
intToReverseList :: Int -> [Int]
intToReverseList 0 = []
intToReverseList x = x `mod` 10 : intToReverseList (x `div` 10)

intToList :: Int -> [Int]
intToList x = reverse (intToReverseList x)

--D
--(a)
settSammen :: [String] -> String
settSammen [] = ""
settSammen [x] = x
settSammen (x:xs) = x ++ [' '] ++ settSammen xs

--(b)
-- There's probably a nicer solution for this, but it works.
delStrengen :: String -> [String]
delStrengen "" = []
delStrengen s = getFirstWord s : delStrengen (dropFirstWord s)

getFirstWord :: String -> String
getFirstWord "" = ""
getFirstWord (x:xs) | x == ' ' = ""
                    | otherwise = x : getFirstWord xs

dropFirstWord :: String -> String
dropFirstWord "" = ""
dropFirstWord (x:xs) | x == ' ' = removeLeadingSpaces xs
                     | otherwise = dropFirstWord xs

removeLeadingSpaces :: String -> String
removeLeadingSpaces "" = ""
removeLeadingSpaces (x:xs) | x == ' ' = removeLeadingSpaces xs
                           | otherwise = x:xs

--(c)
gDelStrengen :: String -> String -> [String]
gDelStrengen xs "" = [xs]
gDelStrengen "" ys = []
gDelStrengen (x:xs) ys | x `erI` ys = gDelStrengen xs ys
                       | otherwise = w : gDelStrengen (drop l (x:xs)) ys
                            where w = takeUntil (x:xs) ys
                                  l = length w

takeUntil :: String -> String -> String
takeUntil "" _ = ""
takeUntil xs "" = xs
takeUntil (x:xs) ys | x `erI` ys = ""
                    | otherwise = x : takeUntil xs ys 

erI :: Char -> String -> Bool
erI _ [] = False
erI c (y:ys) | c == y = True
             | otherwise = c `erI` ys

{-
Thought I was supposed to match the entire second string,
after I was done I realized it was only the individual symbols
in it. Leaving it because it took me way too long.

gDelStrengen :: String -> String -> [String]
gDelStrengen xs "" = [xs]
gDelStrengen "" yp = []
gDelStrengen xs yp = w : gDelStrengen (drop (lw + lp) xs) yp
                        where w = takeUntil xs yp
                              lw = length w
                              lp = length yp

startsWith :: String -> String -> Bool
startsWith _ "" = True
startsWith "" _ = False
startsWith (x:xs) (y:yp) | x == y = startsWith xs yp
                         | otherwise = False

takeUntil :: String -> String -> String
takeUntil "" _ = ""
takeUntil xs "" = xs
takeUntil (x:xs) yp | (x:xs) `startsWith` yp = ""
                    | otherwise = x : takeUntil xs yp
-}