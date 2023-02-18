allsquares :: (Num a) => [a] -> [a]
allsquares [] = []
allsquares (x:xs) = x*x : allsquares xs
