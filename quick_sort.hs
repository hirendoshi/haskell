quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
--quicksort' (x:xs) = quicksort' (filter (<x) xs) ++ [x] ++ quicksort' (filter (>=x) xs)
quicksort' (x:xs) = let l = [ v | v <- xs, v > x ]
                        r = [ v | v <- xs, v <= x ]
                    in quicksort' l ++ [x] ++ quicksort' r
