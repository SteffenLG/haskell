data Tile = T8 | T4 | T2 | T1

instance Show Tile where
    show T8 = "8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8\n8 8 8 8 8 8 8 8"
    show T4 = "4 4 4 4\n4 4 4 4\n4 4 4 4\n4 4 4 4"
    show T2 = tlCorner ++ hLine 4 ++ trCorner ++ "\n" ++ vLine ++ space 4 ++ vLine ++ "\n" ++ blCorner ++ hLine 4 ++ brCorner
    show T1 = tlCorner ++ hLine 1 ++ trCorner ++ "\n" ++ blCorner ++ hLine 1 ++ brCorner

space :: Int -> String
space n = take n $ repeat ' '

hLine :: Int -> String
hLine n = take n $ repeat '─'

vLine :: Int -> String
vLine = "│"

trCorner :: String
trCorner = "┐"

tlCorner :: String
tlCorner = "┌"

brCorner :: String
brCorner = "┘"

blCorner :: String
blCorner = "└"

square :: Int -> String
square n = tlCorner ++ hLine ((n * 2) - 1) ++ "\n" ++ walls n ++ blCorner ++ hLine ((n * 2) - 1) ++ brCorner

walls :: Int -> String
walls 1 = "\n"
walls n = vLine 1 ++ space (2 * n) ++ vLine 1 ++