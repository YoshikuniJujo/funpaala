mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs
