brett :: Int -> IO ()
brett n = do 
    clear
    move (0,0)
    putStrLn $ createBrett n
    putStrLn "\ESC[APut X: `n x y`, Delete X: `d x y`, Quit: `q`"
    readCmds n
    where 
        clear = putStr "\ESC[2J"

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