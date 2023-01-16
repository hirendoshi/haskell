-- all - apply f to all elements of the list. f returns boolean
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and [ f x | x <- xs ]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x == True = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n - 1)  xs

init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = [x] ++ init' xs

data Shape = Circle Float | Rect Float Float | Cube Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y
area (Cube a) = 6 * a^2

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv a b = Just (a `div` b)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)
