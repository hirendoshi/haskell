-- std library functions using foldl and foldr
--
maximum' :: (Ord a) => [a] -> a
maximum' xs = foldl1 (\acc x -> if x > acc then x else acc) xs

reverse' :: [a] -> [a]
reverse' xs = foldr (\x acc -> acc ++ [x] ) [] xs

product' :: (Num a) => [a] -> a
product' = foldr (\x acc -> acc * x) 1

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x:acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1(\_ x -> x)
