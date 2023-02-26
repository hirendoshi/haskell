elemf :: (Eq a) => a -> [a] -> Bool
elemf x xs = foldl (\acc y -> if y == x then True else acc) False xs
