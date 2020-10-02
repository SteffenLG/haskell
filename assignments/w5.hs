import Data.Char
import Data.List

-- 1
data Ast = V Int | O String | P Ast Ast | M Ast Ast

eval :: Ast -> Int
eval (V x) = x
eval (P x y) = (eval x) + (eval y)
eval (M x y) = (eval x) * (eval y)

-- 2
inn :: Ast -> String
inn (V x) = show x
inn (O x) = x
inn (P x y) = "(" ++ (inn x) ++ " + " ++ (inn y) ++ ")"
inn (M x y) = "(" ++ (inn x) ++ " * " ++ (inn y) ++ ")"

-- 3.1
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs) 
        | x == '+' = [[x]] ++ tokenize xs
        | x == '*' = [[x]] ++ tokenize xs
        | x == ' ' = tokenize xs
        | isDigit x = [x : nextDigits] ++ tokenize dRest
        | isLetter x = [x : nextLetters] ++ tokenize lRest
        | otherwise = ["Invalid symbol: " ++ [x]] ++ tokenize xs
    where 
        nextDigits = takeWhile isDigit xs
        nextLetters = takeWhile isLetter xs
        dRest = dropWhile isDigit xs
        lRest = dropWhile isLetter xs

-- 3.2
parse :: String -> Ast
parse xs = let (res, _) = parseTokens (tokenize xs) in res

parseTokens :: [String] -> (Ast, [String])
parseTokens [] = ((O "Reached end of input while parsing"), [])
parseTokens (x:xs) 
        | x == "+" = ((P first second), xs)
        | x == "*" = ((M first second), xs)
        | isNum x = ((V (read x)), xs)
        | otherwise = ((O x), xs)
    where 
        (first, rest) = parseTokens xs
        (second, _) = parseTokens rest
        isWord x = all isLetter x
        isNum x = all isDigit x

-- 3.3
{-
    If the input is structured incorrectly (for example "+ 2" is missing an 
    input for the + operator) the error "Reached end of input while parsing"
    is printed. 
    If an invalid symbol such as "!" is present, "Invalid symbol: !"
    is printed.
    If the input is valid, it is evaluated and the result is returned.
    If the input simply contains a word, the word is printed. All of these
    can occur and be warned about simultaneously.
-}
ev :: String -> Int
ev xs | valid = eval a
      | otherwise = error (intercalate "\n" os)
    where 
        a = parse xs
        os = map inn (getOs a)
        valid = (length os) == 0

getOs :: Ast -> [Ast]
getOs (P x y) = getOs x ++ getOs y
getOs (M x y) = getOs x ++ getOs y
getOs (V _) = []
getOs o = [o]

-- 3.4
infiks :: String -> String
infiks = inn . parse