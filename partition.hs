partition' :: (a -> Bool) -> [a] -> ([a], [a])
--partition' f l = (filter f l, filter (not . f) l)
partition' _ [] = ([], [])
partition' f (x:xs) | f x = (x:ac, rj)
                    | otherwise = (ac, x:rj)
                    where (ac, rj) = partition' f xs
