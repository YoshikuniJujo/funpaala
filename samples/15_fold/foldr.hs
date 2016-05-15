import Funpaala

mySum, myProduct :: [Integer] -> Integer
mySum = fldr (+) 0
myProduct = fldr (*) 1

myLength :: [a] -> Int
myLength = fldr (const (1 +)) 0
