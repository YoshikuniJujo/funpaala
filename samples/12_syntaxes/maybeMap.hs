mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f (Just x) = Just $ f x
mmap _ Nothing = Nothing
