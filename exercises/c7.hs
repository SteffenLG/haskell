--7.9.1
fm :: (a -> a) -> (a -> Bool) -> [a] -> [a]
fm f p xs = (map f . filter p) xs

--7.9.2
--a
alll :: (a -> Bool) -> [a] -> Bool
alll p xs = and (map p xs) 

--b
anyy :: (a -> Bool) -> [a] -> Bool
anyy p xs = or (map p xs)

--c
takeWhilee :: (a -> Bool) -> [a] -> [a]
takeWhilee p (x:xs) | p x = x : takeWhilee p xs
                   | otherwise = []

--d
dropWhilee :: (a -> Bool) -> [a] -> [a]
dropWhilee p (x:xs) | p x = dropWhilee p xs
                    | otherwise = (x:xs)

--7.9.3
mapp :: (a -> b) -> ([a] -> [b])
mapp f = foldr f [] 