import Data.Char
extractDigits :: String -> String
extractDigits [] = []
extractDigits (x:xs) | isDigit x = x : extractDigits xs
                     | otherwise = extractDigits xs
