myHead :: [a] -> a
myHead (x : _) = x

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myNull :: [a] -> Bool
myNull [] = True
myNull (_ : _) = False
