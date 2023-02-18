countOdds :: [Int] -> Int
countOdds [] = 0
countOdds (x:xs) = if odd x then 1 + countOdds xs else countOdds xs
