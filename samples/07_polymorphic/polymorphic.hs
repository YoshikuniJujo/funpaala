myFromMaybe :: a -> Maybe a -> a
myFromMaybe _ (Just x) = x
myFromMaybe d Nothing = d

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe _ f (Just x) = f x
myMaybe d _ Nothing = d

myId :: a -> a
myId x = x

myConst :: a -> b -> a
myConst x _ = x

(.$.) :: (a -> b) -> a -> b
f .$. x = f x

(...) :: (b -> c) -> (a -> b) -> (a -> c)
(f ... g) x = f (g x)

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

myOn :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(op `myOn` f) x y = f x `op` f y
