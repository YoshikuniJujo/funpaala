myFavoriteFruit = "apple"

encrypt m = m ^ 13 `mod` 138689
decrypt c = c ^ 95497 `mod` 138689

twice f x = f (f x)

maybeTriple (Just x) = x * 3
maybeTriple _ = 0

allOrNothing x
	| x > 9 = 100
	| otherwise = 0

num1, num2, num3 :: Integer
num1 = 2
num2 = 8
num3 = 15

calc1 :: Integer -> Integer -> Integer -> Integer
calc1 x y z = x * y + z
