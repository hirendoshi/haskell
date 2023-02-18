import Data.Char    
alltoupper :: String -> String

alltoupper [] = []
alltoupper (x:xs) = toUpper x : alltoupper xs
