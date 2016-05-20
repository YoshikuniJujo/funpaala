import Data.Maybe

prices :: [(String, Integer)]
prices = [
	("apple", 100),
	("banana", 70),
	("orange", 90) ]

myMapMaybe :: (a -> Maybe b) -> [a] -> [b]
myMapMaybe f (x : xs) = case f x of
	Just y -> y : myMapMaybe f xs
	_ -> myMapMaybe f xs
myMapMaybe _ _ = []

justs :: [Maybe a] -> [a]
justs (mx : xs) = case mx of
	Just x -> x : justs xs
	_ -> justs xs
justs _ = []
