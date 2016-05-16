import Data.List
import Data.Bool
import Funpaala

mapRaw, mapF, mapU :: (a -> b) -> [a] -> [b]
mapRaw f (x : xs) = f x : mapRaw f xs
mapRaw _ _ = []

mapF f = fldr ((:) . f) []

mapU f = unfoldr $ \l -> case l of
	x : xs -> Just (f x, xs)
	_ -> Nothing

filterRaw, filterF :: (a -> Bool) -> [a] -> [a]
filterRaw p (x : xs)
	| p x = x : filterRaw p xs
	| otherwise = filterRaw p xs
filterRaw _ _ = []

filterF p = fldr (\x -> bool id (x :) (p x)) []

partitionRaw, partitionF :: (a -> Bool) -> [a] -> ([a], [a])
partitionRaw p (x : xs)
	| p x = (x : ts, es)
	| otherwise = (ts, x : es)
	where (ts, es) = partitionRaw p xs
partitionRaw _ _ = ([], [])

partitionF p = fldr
	(\x (ts, es) -> bool (ts, x : es) (x : ts, es) (p x))
	([], [])
