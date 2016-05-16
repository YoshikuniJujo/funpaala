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
