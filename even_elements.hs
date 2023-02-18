even_elements :: [a] -> [a]

even_elements [] = []
even_elements (x:xs) | xs : even_elements (tail xs)
