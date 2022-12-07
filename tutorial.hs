import Data.Char

in_range :: Int -> Int -> Int -> Bool
in_range min max x = let lb = min <= x
                         ub = x <= max
                     in
                        lb && ub

-- factorial style 1
fac n  | n < 0 = 0
       | n == 0 = 1
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
nub' (x:xs) | elem' x xs == True = nub' xs
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
positions x xs = [ i | (i, x') <- zip' [0..] xs,  x' == x  ]

char2int :: Char -> Int
char2int c = ord c  - ord 'a'

int2char :: Int -> Char
int2char i = chr(ord 'a' + i)

shift :: Int -> Char -> Char
shift n c | isLower c = int2char( (char2int(c) + n)  `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs ]

-- Nested loops
-- Tuples of 0 <= x <= m and 0 <= y << n for all (x, y)
grid :: Int -> Int -> [(Int, Int)]
grid m n = [ (x, y) | x <- [0..m], y <- [0..n] ]

-- replicate Int n times
replicate' :: Int -> a -> [a]
replicate' n a = [ a | _ <- [1..n] ]

-- Pythagorian triples
pyths :: Int -> [(Int, Int, Int)]
pyths n = [ (a, b, c) | a <- [1..n], b <- [1..n], c <- [b..n], c^2 == a^2 + b^2 ]

-- Factors of a number n
factors' :: Int -> [Int]
factors' n = [ x | x <- [1..n], n `mod` x == 0 ]

-- Is the number a prime number
isPrime' :: Int -> Bool
isPrime' n = factors' n == [1, n]

-- Perfect first n numbers
perfect' :: Int -> [Int]
perfect' n = [ x | x <- [1..n], sum (init (factors'  x)) == x ]

-- Length of a list
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs Prelude.++ [x]

-- Concat two lists
(+#) :: [a] -> [a] -> [a]
[] +# ys  = ys
(x:xs) +# ys = x : (xs +# ys)

-- Insert a elemement in a sorted array
insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs) | e < x = e : x : xs
                | otherwise = x : insert e xs


-- Insertion sort
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

-- nth fibonacci number
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

-- mutual recursion of even and odd
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

-- Even or Odd index positions - starting with zero index
getEvens :: [a] -> [a]
getEvens [] = []
getEvens (x:xs) = x : getOdds xs 

getOdds :: [a] -> [a]
getOdds [] = []
getOdds (_:xs) = getEvens xs

-- init, remove last element of a list
init' :: [a] -> [a]
init' [] = []
init' [_] = []
init' (x:xs) = x : init' xs

-- sumdown. add numbers from n, n-1, ... 0
sumdown :: Int -> Int
sumdown n | n == 0 = 0
          | otherwise = n + sumdown (n - 1)

-- Exponential operator
(^#) :: Int -> Int -> Int
_ ^# 0 = 1
a ^# b = a * (a ^# (b-1))

-- Euclid gcd - subtract smaller number from larger up until they are equal
euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a < b = euclid a (b-a)
           | otherwise = euclid (a-b) b

-- ands' all True in list
ands' :: [Bool] -> Bool
ands' [] = True
ands' [x] = x
ands' (x:xs) | x == False = False
             | otherwise = ands' xs

-- concatanate list of lists
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- replicate a n times
repls' :: Int -> a -> [a]
repls' 0 _ = []
repls' n x = [x] ++ repls' (n-1) x

-- select the nth elemment of a list
(!#) :: [a] -> Int -> a
(x:_) !# 0 = x
(_:xs) !# n = xs !# (n-1)

-- is element e in the list
elems :: Eq a => a -> [a] -> Bool
elems _ [] = False
elems e (x:xs) | e == x = True
               | otherwise = elems e xs

-- merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- Non recursive map
mapnr :: (a -> b) -> [a] -> [b]
mapnr _ [] = []
mapnr f xs = [ f x | x <- xs ]

-- Non recursive filter
filternr :: (a -> Bool) -> [a] -> [a]
filternr f xs = [ x | x <- xs, f x ]

-- takeWhile predicate is true
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) | f x = [x] ++ takeWhile' f xs
                    | otherwise = []

-- dropWhile predicate is true
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x =  dropWhile' f xs
                    | otherwise = x:xs

-- or on a list
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

-- and on a list
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- foldr fold right 
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

-- Additon of Int
(+!) :: Int -> Int -> Int
a +! b = a + b


-- simulate foldl sum
suml :: Num a => [a] -> a
suml = suml' 0
       where suml' v []  = v
             suml' v (x:xs) = suml' (v+x) xs

-- Composition
-- Apply a function twice
twice f = f . f
-- to be continued
--

