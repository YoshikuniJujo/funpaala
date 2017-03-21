dupHead :: [a] -> [a]
dupHead xxs@(x : _) = x : xxs
dupHead _ = []
