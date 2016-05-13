myHead :: [a] -> a
myHead (x : _) = x
myHead _ = error "Bonehead!"

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False
