leftreduce :: (Num a) => a -> [a] -> a
leftreduce n [] = n
leftreduce n (x:xs) = leftreduce (n + x) xs
