--data Sudoku = [Int]
data Triple = Triple (Int, Int, Int)
data Sudoku = Triple (Triple (Triple (Triple Symbol)))

--readSudoku IO () -> Sudoku

printSudoku :: Sudoku -> IO ()
printSudoku = parseSudoku . readLn

parseSudoku :: String -> Sudoku
parseSudoku = (Sudoku foldr () $ zip [0..]

