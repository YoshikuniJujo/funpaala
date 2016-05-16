dupHead :: [a] -> [a]
dupHead (x : xs) = x : x : xs
dupHead _ = []
