intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' _ [x] = x
intercalate' s (x:xs) = x ++ s ++ intercalate' s xs
