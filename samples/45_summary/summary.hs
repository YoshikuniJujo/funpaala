import Data.Char
import Data.List

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

val1 :: Integer
val1 = x ^ y
	where
	x = 3
	y = 4

checkAnswer :: Char -> Maybe Bool
checkAnswer c = case toLower c of
	'y' -> Just True
	'n' -> Just False
	_ -> Nothing

aho :: Integer -> String
aho x = if x `mod` 3 == 0 then "aho" else "normal"

type Point = (Double, Double)

geometric :: Integer -> Integer
geometric n
	| n < 1 = 1
	| otherwise = 2 * geometric (n - 1)

mySum :: [Integer] -> Integer
mySum (x : xs) = x + mySum xs
mySum _ = 0

enumerate :: Integer -> [Integer]
enumerate n = n : enumerate (n + 1)

cookie :: Integer -> [Integer]
cookie = unfoldr $ \s ->
	if s < 3 then Nothing else let b = s `div` 3 in Just (b, s - b)

fibs, tfibs :: [Integer]
fibs@(_ : tfibs) = 0 : 1 : zipWith (+) fibs tfibs
