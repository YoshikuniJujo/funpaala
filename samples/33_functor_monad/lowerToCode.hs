import Data.Char (isLower, ord)

lowerToCode :: Char -> Maybe Int
lowerToCode c
	| isLower c = Just $ ord c
	| otherwise = Nothing

evenDiv2 :: Int -> Maybe Int
evenDiv2 n
	| even n = Just $ n `div` 2
	| otherwise = Nothing

bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
Just x `bindM` f = f x
Nothing `bindM` _ = Nothing
