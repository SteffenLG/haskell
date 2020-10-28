sumStats :: (String, Int, Int) -> Int
sumStats (_, x, y) = x + y


claramentePior :: (String, Int, Int) -> [(String, Int, Int)] -> Bool
claramentePior p g 
        | null g = True
        | (sumStats p) > (sumStats (head g)) = False
        | otherwise = claramentePior p (tail g)