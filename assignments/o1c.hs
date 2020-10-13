-- Steffen Lid Gaustad - Group 4
module Oblig1 where
import Data.Char
data Ast
    = Word String
    | Num Int
    | Mult Ast Ast
    | Plus Ast Ast
    | Minus Ast Ast
    deriving (Eq, Show)

-- 1
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs) 
    | x == '+' = "+" : tokenize xs
    | x == '-' = "-" : tokenize xs
    | x == '*' = "*" : tokenize xs
    | x == ' ' = tokenize xs
    | isDigit x = [x : nextDigits] ++ tokenize dRest
    | isLetter x = [x : nextLetters] ++ tokenize lRest
    | otherwise = ["Invalid symbol: " ++ [x]] ++ tokenize xs
    where 
        nextDigits = takeWhile isDigit xs
        nextLetters = takeWhile isLetter xs
        dRest = dropWhile isDigit xs
        lRest = dropWhile isLetter xs

parse :: String -> Ast
parse str = res 
    where (res, _) = parsePlus (tokenize str)

parsePlus :: [String] -> (Ast, [String]) 
parsePlus s 
    | null z = (a,z)
    | x == "+" = (Plus a c, r)
    | otherwise = error "Parsing failed"
    where
        (a, z) = parseMinus s
        x = head z
        (c, r) = parsePlus(tail(z))

parseMinus :: [String] -> (Ast, [String]) 
parseMinus s
    | null z = (a,z)
    | x == "-" = (Minus a c, r)
    | otherwise = (a,z)
    where 
        (a, z) = parseMult s
        x = head z
        (c, r) = parsePlus(tail(z))

parseMult :: [String] -> (Ast, [String]) 
parseMult s
    | null z = (a,z)
    | x == "*" = if isNum a then (Mult a c, r) else error "Parsing Error: Mult needs Num as first param"
    | otherwise = (a,z)
    where
        (a,z) = parseVal s
        x = head z
        (c, r) = parsePlus(tail(z))
        isNum (Num _) = True
        isNum _ = False

parseVal :: [String] -> (Ast, [String])
parseVal (x:xs) 
    | isNum x = (Num (read x), xs)
    | isWord x = (Word x, xs)
    where
        isWord x = all isLetter x
        isNum x = all isDigit x

-- 2
viss :: Ast -> String
viss a = (vish a 0) ++ "\n"

vish :: Ast -> Int -> String
vish (Num x) n = spaces n ++ "Num " ++ show x
vish (Word x) n = spaces n ++ "Word " ++ x
vish (Plus x y) n = spaces n ++ "Plus\n" ++ (vish x (n + 1)) ++ "\n" ++ (vish y (n + 1)) 
vish (Mult x y) n = spaces n ++ "Mult\n" ++ (vish x (n + 1)) ++ "\n" ++ (vish y (n + 1))
vish (Minus x y) n = spaces n ++ "Minus\n" ++ (vish x (n + 1)) ++ "\n" ++ (vish y (n + 1)) 

spaces :: Int -> String    
spaces 0 = ""
spaces x = "   " ++ spaces (x - 1)

vis :: Ast -> IO ()
vis ast = putStr (viss ast)

-- 3 
eval :: Ast -> String
eval (Word x) = x
eval x = eval (evalAst x)

evalAst :: Ast -> Ast
evalAst (Plus  (Num _) _) = error "Cannot plus Nums"
evalAst (Plus  _ (Num _)) = error "Cannot plus Nums"
evalAst (Mult (Word _) _) = error "Mult requires Num as first parameter"
evalAst (Mult  _ (Num _)) = error "Mult requires Word as second parameter"
evalAst (Minus  (Num _) _) = error "Cannot minus Nums"
evalAst (Minus  _ (Num _)) = error "Cannot minus Nums"
evalAst (Word a) = Word a
evalAst (Num a) = Num a
evalAst (Plus  (Word a) (Word b)) = Word (a ++ b)
evalAst (Mult  (Num  a) (Word b)) = Word (multiply a b)
evalAst (Minus (Word a) (Word b)) = Word (removeAll a b)
evalAst (Plus  a b) = (Plus  (evalAst a) (evalAst b))
evalAst (Mult  a b) = (Mult  (evalAst a) (evalAst b))
evalAst (Minus a b) = (Minus (evalAst a) (evalAst b))

removeAll :: String -> String -> String
removeAll "" _ = ""
removeAll xs "" = xs
removeAll xs (y:ys) = removeAll (removeFirst y xs) ys

removeFirst :: Char -> String -> String
removeFirst _ [] = []
removeFirst c (x:xs) 
    | x == c = xs
    | otherwise = x : removeFirst c xs

multiply :: Int -> String -> String
multiply 0 _ = ""
multiply 1 xs = xs
multiply n xs = xs ++ multiply (n - 1) xs