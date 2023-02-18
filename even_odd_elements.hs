 even_elements :: [a] -> [a]
 even_elements [] = []
 even_elements (x:xs) = x : odd_elements xs

 odd_elements :: [a] -> [a]
 odd_elements [] = []
 odd_elements (_:xs) = even_elements xs
