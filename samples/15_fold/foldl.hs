sumIter :: Integer -> [Integer] -> Integer
sumIter s (x : xs) = sumIter (s + x) xs
sumIter s _ = s

mySum :: [Integer] -> Integer
mySum = sumIter 0
