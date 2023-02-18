lastButOne :: [a] -> Maybe a

lastButOne [] = Nothing
lastButOne (_:[]) = Nothing
lastButOne (x:(_:[])) = Just x
lastButOne (x:xs) = lastButOne xs
