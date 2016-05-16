dupHead :: [a] -> [a]
dupHead xa@(x : _) = x : xa
dupHead _ = []
