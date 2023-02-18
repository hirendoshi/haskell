zipwith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipwith' _ [] _ = []
zipwith' _ _ [] = []
zipwith' f (x:xs) (y:ys) = f x y : (zipwith' f xs ys)
