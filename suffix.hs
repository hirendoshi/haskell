suffix :: String -> [String]
suffix "" = []
suffix s  = s : suffix (tail s)
