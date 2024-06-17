prms :: Int -> [Int]
prms n = rmv[2..n]

rmv :: [Int] -> [Int]
rmv [] = []
rmv (x:xs) = [x] ++ rmv2 x xs

rmv2 :: Int -> [Int] -> [Int]
rmv2 _ [] = []
rmv2 x (y:ys) | y `mod` x == 0 = rmv2 x ys
              | otherwise = [y] ++ rmv2 x ys
