dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
--- dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs
dropWhile' p (x:xs) | p x = dropWhile' p xs
                    | otherwise = x:xs
