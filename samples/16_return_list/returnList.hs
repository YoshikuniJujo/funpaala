import Data.List (unfoldr)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f s = case f s of
	Nothing -> []
	Just (x, s') -> x : myUnfoldr f s'

dict :: [(Integer, (Char, Integer))]
dict = [
	(5, ('k', 11)),
	(7, ('l', 3)),
	(0, ('H', 9)),
	(17, ('s', 5)),
	(9, ('a', 17)),
	(11, ('e', 7)),
	(3, ('l', 20)) ]

takeTo :: (a -> Bool) -> [a] -> [a]
takeTo p = unfoldr $ \s -> case s of
	[] -> Nothing
	x : xs	| p x -> Just (x, [])
		| otherwise -> Just (x, xs)
