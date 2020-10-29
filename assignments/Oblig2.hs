data State = State {
    size :: Int,
    cells :: [(Int, Int)],
    rules :: Rules,
    message :: String,
    running :: Bool,
    showGame :: Bool
}

data Rules = Rules {
    surv :: (Int, Int),
    bec :: (Int, Int)
} deriving Show

defaultState :: State
defaultState = State (GameState n [] (Rules (2,3) (3,3))) "Welcome to the Game of life" True False

brett :: Int -> IO ()
brett n = do 
    clear
    move (0,0)
    putStrLn $ createBrett n
    readCmds n
    where 
        clear = putStr "\ESC[2J"

main :: Int -> IO ()
main n = do
    brett n
    run defaultState

run :: State -> IO ()
run s = do
    ln <- getLine
    s' <- cmdMap ln s
    putStrLn $ message s'
    clear s
    update s'
    if running s then 
        do run s'
    else
        do return ()

cmdMap :: String -> State -> State
cmdMap "" = 
cmdMap xs = case cmd of 
    "c"    -> c $ head $ p args
    "n"    -> n $ p args
    "e"    -> e $ p args
    "b"    -> b $ p args
    "s"    -> s $ p args
    "?"    -> ?
    "w"    -> w
    "r"    -> r $ head args
    "l"    -> l $ read $ head args
    "quit" -> q
    _      -> er (cmd ++ " is not a valid command")
    where
        w = words xs
        cmd = head w
        args = tail w
        p x = map read x

c :: Int -> State -> State
c n s = 

n :: [Int] -> State -> State
n xs s = (s { cells = (cells s) ++ (tuplify xs) })

e :: [Int] -> State -> State
e xs s = (s { cells = lMinus (cells s) xs })

b :: [Int] -> State -> State
b (x:y:xs) s = let r = rules s in 
    (s { rules = r { bec = (x, y) } })
b _ s = s

s :: [Int] -> State -> State
s (x:y:xs) st = let r = rules st in 
    (st { rules = r { surv = (x, y) } })
s _ st = st


? :: State -> State
? s = s { message = show $ rules s }

w :: State -> State


r :: String -> State -> State

-- Might need special case for this
l :: Int -> State -> State

er :: String -> State -> State

tuplify :: [Int] -> [(Int, Int)]
tuplify (x:y:xs) = (x, y) : tuplify xs
tuplify _ = []

lMinus :: Eq a => [a] -> [a] -> [a]
lMinus xs ys = [x | x <- xs, not (find x ys)]

find _ [] = False
find x (x:xs) = True
find x (_:xs) = find x xs




readCmds :: Int -> IO ()
readCmds n = do
    ln <- getLine
    let l = words ln
        cmd = head l
        args = tail l
        mv = move (parseArgs args)
    if cmd == "n" then do
        mv
        putStr "X"
    else if cmd == "d" then do
        mv
        putStr "."
    else if cmd == "q" then do 
        putStr "Good bye"
    else do 
        putStr ("Invalid command: " ++ cmd)
    if cmd == "q" then do 
        return ()
    else do
        move (0, n + 2)
        clearBelow
        readCmds n
    where 
        w = width n
        parseArgs [x, y] = ((((read x) + 1) * (w + 1)) + 1, (read y) + 1)
        clearBelow = putStrLn "\ESC[0J"
        


move :: (Int, Int) -> IO ()
move (x, y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")

createBrett :: Int -> String
createBrett n = (padStr (w + 1) "") ++ (row w (map show [1..n])) ++ rows w n
    where w = 1 + width n

width = length . show 

row :: Int -> [String] -> String
row w i = concat (map (padStr w) i) ++ "\n"

rows :: Int -> Int -> String
rows w n = concat (map (\x -> (padStr w (show x)) ++ " " ++ (row w (take n $ repeat "."))) [1..n])

padStr :: Int -> String -> String
padStr l s = if length s == l then s else padStr l (" " ++ s)