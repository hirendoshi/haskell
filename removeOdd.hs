removeOdd :: [Int] -> [Int]
removeOdd [] = []
removeOdd (x:xs) = if odd x then removeOdd xs else x : removeOdd xs
