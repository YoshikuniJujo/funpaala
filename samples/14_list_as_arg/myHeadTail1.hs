myHead :: [a] -> a
myHead (x : _) = x
myHead [] = error "Bonehead!"

myTail :: [a] -> [a]
myTail (_ : xs) = xs

myNull :: [a] -> Bool
myNull [] = True
myNull (_ : _) = False
