import Funpaala

len :: a -> Int -> Int
-- len _ l = 1 + l
len = const (1 +)

mySum, myProduct :: [Integer] -> Integer
mySum = fldr (+) 0
myProduct = fldr (*) 1

myLength :: [a] -> Int
myLength = fldr (const (1 +)) 0

myFldr :: (a -> b -> b) -> b -> [a] -> b
myFldr op v (x : xs) = x `op` myFldr op v xs
myFldr _ v [] = v

sumIter :: Integer -> [Integer] -> Integer
sumIter s [] = s
sumIter s (x : xs) = sumIter (s + x) xs

mySumL :: [Integer] -> Integer
mySumL = sumIter 0

myFldl :: (a -> b -> a) -> a -> [b] -> a
myFldl op s (x : xs) = myFldl op (s `op` x) xs
myFldl _ s [] = s

myFldl' :: (a -> b -> a) -> a -> [b] -> a
myFldl' _ s [] = s
myFldl' op s (x : xs) = s `seq` myFldl' op (s `op` x) xs

ffldr :: (a -> b -> b) -> [a] -> b -> b
-- ffldr op = flip $ fldr op
-- ffldr op (x : xs) v = x `op` ffldr op xs v
-- ffldr _ [] v = v
ffldr op (x : xs) = op x . ffldr op xs
ffldr _ [] = id

sumCord :: (Char -> Int) -> [Char] -> Int
sumCord f (c : cs) = f c + sumCord f cs
sumCord _ [] = 0
