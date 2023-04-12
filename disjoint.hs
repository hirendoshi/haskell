disjoint ::  [Int] -> [Int] -> Bool
disjoint [] _ = True
disjoint _ [] = True
disjoint (x:xs) (y:ys) | x == y = False
                       | x < y = disjoint xs (y:ys)
                       | otherwise = disjoint (x:xs) ys

disjoint' :: (Ord a) => [a] -> [a] -> Bool
disjoint' _ [] = True
disjoint' [] _ = True
disjoint' [x] (y:ys) | x == y = False
                     | otherwise = True
disjoint' (x:xs) [y] | x == y = False
                     | otherwise = True
