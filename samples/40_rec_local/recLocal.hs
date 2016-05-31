fix :: (a -> a) -> a
fix f = f (fix f)

fix2 :: (a -> b -> a) -> (b -> a -> b) -> a
fix2 f g = f (fix2 f g) (fix2 g f)
