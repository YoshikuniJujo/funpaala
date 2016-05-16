import Data.List

takeTo p = unfoldr $ \s -> case s of
	[] -> Nothing
	x : xs	| p x -> Just (x, [])
		| otherwise -> Just (x, xs)
