disjoint' :: (Ord a) => [a] -> [a] -> Bool
disjoint' _ [] = True
disjoint' [] _ = True
disjoint' [x] (y:ys) | x == y = False
                     | otherwise = True
disjoint' (x:xs) [y] | x == y = False
                     | otherwise = True

