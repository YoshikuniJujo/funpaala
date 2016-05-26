import Data.Monoid

import Funpaala

newtype Max a = Max { getMax :: a } deriving Show

instance (Ord a, Bounded a) => Monoid (Max a) where
	mempty = Max minBound
	Max x `mappend` Max y = Max $ x `max` y

ffldr :: (a -> b -> b) -> [a] -> b -> b
ffldr =  flip . fldr

fldMap :: Monoid m => (a -> m) -> [a] -> m
fldMap f (x : xs) = f x `mappend` fldMap f xs
fldMap _ _ = mempty

ffldr' :: (a -> b -> b) -> [a] -> b -> b
ffldr' f = appEndo . fldMap (Endo . f)

fldMap' :: Monoid m => (a -> m) -> [a] -> m
fldMap' f = fldr (mappend . f) mempty
