primes :: Int -> [Int]
primes n = wrap n [2..n]

wrap :: Int -> [Int] -> [Int]
wrap n [x] = []
wrap n (x:xs) = remove x xs

remove :: Int -> [Int] -> [Int]
remove _ [] = []
remove n (x:xs) | x `mod` n == 0 = remove n xs
                | otherwise = remove n (x:xs)
