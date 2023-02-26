srch :: (Eq a) => a -> Int -> [a] -> [Int]
srch _ n [] = []
srch e n (x:xs) | x == e = n : srch e (n + 1) xs
                | otherwise = srch e (n + 1) xs

elemIndices' :: (Eq a) => a -> [a] -> [Int]
elemIndices' elem ex = srch elem 0 ex
