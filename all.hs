all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) | f x = True
              | otherwise = False
