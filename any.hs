any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) | f x = True
              | otherwise = any' f xs
