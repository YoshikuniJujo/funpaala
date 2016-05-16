myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)
