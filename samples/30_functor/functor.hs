import Data.Tree

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f (Just x) = Just $ f x
mmap _ _ = Nothing

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Node x sf) = Node (f x) $ map (tmap f) sf

newtype Op a = Op { op :: Int -> Int -> a }

instance Functor Op where
	fmap f (Op o) = Op $ \x y -> f $ x `o` y

add3 :: Op Int
add3 = (* 3) `fmap` Op (+)

dividable :: Op Bool
dividable = (== 0) `fmap` Op mod
