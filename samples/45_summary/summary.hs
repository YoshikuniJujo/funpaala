myFavoriteFruit = "apple"

encrypt m = m ^ 13 `mod` 138689
decrypt c = c ^ 95497 `mod` 138689

twice f x = f (f x)

maybeTriple (Just x) = x * 3
maybeTriple _ = 0

allOrNothing x
	| x > 9 = 100
	| otherwise = 0
