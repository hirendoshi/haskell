takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
--takeWhile' f (x:xs) = if f x then x : takeWhile' f xs else f []
takeWhile' f (x:xs) | f x == True = x : takeWhile' f xs
                    | otherwise = []
