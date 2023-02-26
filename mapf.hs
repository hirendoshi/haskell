mapf :: (a -> b) -> [a] -> [b]
mapf f xs = foldr (\y acc -> f y : acc) [] xs
