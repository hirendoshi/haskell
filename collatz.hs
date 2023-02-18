collatz :: Int -> [Int]
collatz 1 = [1]
collatz n | even n == True = n : collatz (n `div` 2)
          | otherwise = n : collatz (n * 3 + 1)
