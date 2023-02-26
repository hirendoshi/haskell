group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' xs = takeWhile (==(head xs)) xs : group' (dropWhile (==head(xs)) xs)
