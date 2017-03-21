import Data.Bool (bool)

takeTo :: (a -> Bool) -> [a] -> [a]
{-
takeTo _ [] = []
takeTo p (x : xs)
	| p x = [x]
	| otherwise = x : takeTo p xs
	-}
takeTo p = foldr (\x -> (x :) . bool id (const []) (p x)) []

collatzNext :: Integer -> Integer
collatzNext n
	| even n = n `div` 2
	| otherwise = n * 3 + 1

collatzInf :: Integer -> [Integer]
collatzInf n = n : collatzInf (collatzNext n)
