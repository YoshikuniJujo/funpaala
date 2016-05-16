takeTo :: (a -> Bool) -> [a] -> [a]
takeTo _ [] = []
takeTo p (x : xs)
	| p x = [x]
	| otherwise = x : takeTo p xs

collatzInf :: Integer -> [Integer]
collatzInf = iterate $ \n -> if even n then n `div` 2 else n * 3 + 1
