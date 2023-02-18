natsum :: Int -> Int
natsum 0 = 0
natsum n | n > 0 = n + natsum (n-1)
         | otherwise = error "negative sum"
