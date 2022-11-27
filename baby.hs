doubleMe x = x + x
doubleUs x y = 2*x + 2*y
doubleSmallNumber x = if x > 100 then x else x * 2
removeNonUpperCase xs = [x | x <-xs, x `elem` ['A'..'Z']]
qsort [] = []
qsort (x : xs) =  qsort a ++ [x] ++ qsort b 
    where 
    a = [a | a <- xs, a <= x]
    b = [b | b <- xs, b > x]

double x = x + x
quadruple x = double (double x)
factorial n = product[1..n]
average xs = sum xs `div` length xs
n = a `div` (length xs)
    where
    a = 10
    xs = [1,2,3,4,5]

square' x y = x * x + y*y
add' x y = x + y

mult' a b c = a * b * c

sig x | x < 0 = -1
      | x == 0 = 0
      | otherwise = 1

mnot b = not b

safetail_a xs = if null xs then [] else tail xs

safetail_b xs | null xs = []
              | otherwise = tail xs

safetail_c [] = []
safetail_c (_:xs) = xs

False || b = b
True  || _ = True

True && b = b
False && _ = False

and' x y = if x then y else False

factors n = [x | x <- [1..n], n `mod` x == 0]
perfect n = [x | x <- [1..n], sum(init(factors x)) == x]
triple n = [(a,b,c) | c <- [n, n-1..1], b <- [c-1, c-2..1], a <- [b-1, b-2..1], c^2 == a^2 + b^2]

fac n | n < 2 = 1
      | otherwise = n * fac(n-1)

pr [] = 1
pr (x:xs) = x * pr xs

fact 0 = 1
fact n = n * fact(n-1)

len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop _ [] = []
mydrop n (_:xs) = drop (n - 1) xs 

(#+) :: [a] -> [a] -> [a]
[] #+ ys = ys
(x:xs) #+ ys = x : (xs #+ ys)

qs :: Ord a => [a] -> [a]
qs [] = []
qs (x:xs) = qs small #+ [x] #+ qs large
        where
        small = [s | s <- xs, s <= x]
        large = [l | l <- xs, l > x]

ands' :: [Bool] -> Bool
ands' [] = True
ands' (b:bs) = b Prelude.&& (ands' bs)

concat' ::  [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n-1) a

(!$) :: [a] -> Int -> a
(x:_) !$ 0 = x
(_:xs) !$ n = xs !$ (n-1)

insert :: Int -> [Int] -> [Int]
insert a [] = [a]
insert a (x:xs) = if a < x then
                    a : x : xs
                  else
                    x : insert a xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert x (isort xs)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then 
                        x : merge xs (y:ys)
                      else
                        y : merge (x:xs) ys

halve :: [Int] -> ([Int], [Int])
halve [] = ([], [])
halve xs = (take s xs, drop s xs)
            where s = length xs `div` 2

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort ys) (msort zs)
           where (ys, zs) = halve(xs)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x = x : filter' f xs
                 | otherwise = filter' f xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

length' = foldr' (\_ n -> 1 + n) 0

reverse' = foldr' (\ x xs -> xs ++ [x]) []

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

dropit :: Int -> [a] -> [a]
dropit 0 xs = xs
dropit _ [] = []
dropit n (_:xs) = dropit (n-1) xs

init' :: [a] -> [a]
init' [] = []
init' (x:xs) | null xs =  [] 
             | otherwise = x : init' xs

data Nat = Zero | Succ Nat
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n



