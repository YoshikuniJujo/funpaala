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

takeRaw, takeU :: Int -> [a] -> [a]
takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

takeU = curry . unfoldr $ \nl -> case nl of
	(n, x : xs) | n > 0 -> Just (x, (n - 1, xs))
	_ -> Nothing

dropRaw :: Int -> [a] -> [a]
dropRaw n (_ : xs) | n > 0 = dropRaw (n - 1) xs
dropRaw _ xs = xs

splitAtRaw :: Int -> [a] -> ([a], [a])
splitAtRaw n (x : xs) | n > 0 = (x : t, d)
	where (t, d) = splitAtRaw (n - 1) xs
splitAtRaw _ xs = ([], xs)

takeWhileRaw, takeWhileF, takeWhileU :: (a -> Bool) -> [a] -> [a]
takeWhileRaw p (x : xs) | p x = x : takeWhileRaw p xs
takeWhileRaw _ _ = []

takeWhileF p = fldr (\x -> bool (const []) (x :) (p x)) []

takeWhileU = undefined

dropWhileRaw :: (a -> Bool) -> [a] -> [a]
dropWhileRaw p (x : xs) | p x = dropWhileRaw p xs
dropWhileRaw _ xs = xs

spanRaw :: (a -> Bool) -> [a] -> ([a], [a])
spanRaw p (x : xs) | p x = (x : t, d)
	where (t, d) = spanRaw p xs
spanRaw _ xs = ([], xs)

zipRaw, zipU :: [a] -> [b] -> [(a, b)]
zipRaw (x : xs) (y : ys) = (x, y) : zipRaw xs ys 
zipRaw _ _ = []

zipU = curry . unfoldr $ \l -> case l of
	(x : xs, y : ys) -> Just ((x, y), (xs, ys))
	_ -> Nothing

zipWithRaw, zipWithU :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRaw op (x : xs) (y : ys) = x `op` y : zipWithRaw op xs ys
zipWithRaw _ _ _ = []

zipWithU op = curry . unfoldr $ \l -> case l of
	(x : xs, y : ys) -> Just (x `op` y, (xs, ys))
	_ -> Nothing

zipZW :: [a] -> [b] -> [(a, b)]
zipZW = zipWith (,)

unzipRaw, unzipF :: [(a, b)] -> ([a], [b])
unzipRaw ((x, y) : xys) = (x : xs, y : ys)
	where (xs, ys) = unzipRaw xys
unzipRaw _ = ([], [])

unzipF = fldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], [])

(.++), (.++.), (.++..) :: [a] -> [a] -> [a]
(x : xs) .++ ys = x : (xs .++ ys)
[] .++ ys = ys

-- xs .++. ys = fldr (:) ys xs
(.++.) = flip $ fldr (:)

(.++..) = curry . unfoldr $ \xys -> case xys of
	(x : xs, ys) -> Just (x, (xs, ys))
	(_, y : ys) -> Just (y, ([], ys))
	_ -> Nothing

concatRaw, concatF :: [[a]] -> [a]
concatRaw (xs : xss) = xs ++ concatRaw xss
concatRaw _ = []

concatF = foldr (++) []

reverseRaw, reverseF :: [a] -> [a]
reverseRaw = rv []
	where
	rv rs (x : xs) = rv (x : rs) xs
	rv rs _ = rs

reverseF = fldl' (flip (:)) []

repeatRaw, repeatU :: a -> [a]
repeatRaw x = x : repeatRaw x

repeatU = unfoldr $ \x -> Just (x, x)

replicateRaw, replicateU :: Int -> a -> [a]
replicateRaw n x | n > 0 = x : replicateRaw (n - 1) x
replicateRaw _ _ = []

replicateU = curry . unfoldr $
	(\(n, x) -> bool Nothing (Just (x, (n - 1, x))) (n > 0))

cycleRaw, cycleC :: [a] -> [a]
cycleRaw xs = xs ++ cycleRaw xs

cycleC = concat . repeat
