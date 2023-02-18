largest_divisible :: (Integral a) => a
largest_divisible = head (filter (\x -> x `mod` 3829 == 0) [10000, 9999..])
