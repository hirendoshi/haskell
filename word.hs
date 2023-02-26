import Data.Char
words' :: String -> [String]
words' "" = []
--words' w = [takeWhile (/= ' ') w] ++ words' (dropWhile (== ' ') $ dropWhile (/= ' ') w)
words' w = [takeWhile (not . isSpace) w] ++ words' (dropWhile isSpace $ dropWhile (not . isSpace) w)
