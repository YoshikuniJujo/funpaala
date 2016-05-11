myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y
