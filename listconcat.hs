(+++) :: [a] -> [a] -> [a]
(+++) [] y = y
(+++) (x:xs) y = x : (xs +++ y)
