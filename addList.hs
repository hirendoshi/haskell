infixr 5 .++
(.++) :: [a] -> [a] -> [a]
xs .++ [] = xs
[] .++ ys = ys
(x:xs) .++ ys = x : (xs .++ ys)
