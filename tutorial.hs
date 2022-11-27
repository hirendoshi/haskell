import Data.Char

in_range :: Int -> Int -> Int -> Bool
in_range min max x = let lb = min <= x
                         ub = x <= max
                     in
                        lb && ub

-- factorial style 1
fac n  | n == 0 = 1
       | otherwise = n * fac (n-1)


-- factorial tail recursion
faculty n = aux n 1
    where
        aux n acc 
            | n == 0 = acc
            | otherwise = aux (n-1) (acc*n)

-- ascending list like [a..n]
asc n m | n > m = []
        | n == m = [n]
        | n < m = n : asc (n+1) m

-- qsort
qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
            where
                smaller = [a | a <- xs, a <= x]
                larger  = [b | b <- xs, b  > x]


-- pattern matching
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

prd' :: [Int] -> Int
prd' [] = 1
prd' (x:xs) = x * prd' xs

sums :: [Int] -> Int
sums xs | xs == [] = 0 
        | otherwise = head xs + sums (tail xs)

evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) | mod x 2 == 0 = x : evens(xs)
             | otherwise = evens xs

addTuples :: [(Int, Int)] -> [Int]
addTuples xs = [ x + y | (x, y) <- xs ]

-- Exercises
-- # 1 Does element e exist in list
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs) | x == e = True 
               | otherwise = elem' e xs


-- #2 Remove duplicates
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
--nub' (x:xs) = if not (elem' x xs) then x:nub'(xs) else nub'(xs)
nub' (x:xs) | elem' x xs = nub' xs
            | otherwise = x : nub' xs

-- #3 Is ascending order list
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x:y:xs) = (x <= y) && isAsc (y:xs)

-- #4 Path from node x to node y
--hasPath :: [(Int, Int)] -> Int -> Int -> Bool
--hasPath [] x y = x == y
--hasPath xs x y | x == y
-- Annonymous function
app :: (a -> b) -> a -> b
app f x = f x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x == True = x : filter' f xs 
                 | otherwise = filter' f xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (i, x') <- zip [0..] xs,  x' == x  ]

char2int :: Char -> Int
int2char :: Int -> Char
char2int c = ord c  - ord 'a'
int2char i = chr(ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char( (char2int(c) + n)  `mod` 26)
          | otherwise = c

