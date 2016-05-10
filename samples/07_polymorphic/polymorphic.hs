myFromMaybe :: a -> Maybe a -> a
myFromMaybe _ (Just x) = x
myFromMaybe d _ = d

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe _ f (Just x) = f x
myMaybe d _ _ = d

myId :: a -> a
myId x = x
