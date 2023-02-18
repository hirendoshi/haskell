import Data.Char

string2int :: String -> Int
string2int s = string2intacc 0 s
    where 
        string2intacc :: Int -> String -> Int
        string2intacc acc [] = acc
        string2intacc acc (x:xs) = string2intacc (10 * acc + digitToInt x) xs
