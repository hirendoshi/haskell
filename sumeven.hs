sumeven :: [Int] -> Int
sumeven [] = 0
sumeven (x:xs) | even x = x + sumeven xs 
               | otherwise = sumeven xs
