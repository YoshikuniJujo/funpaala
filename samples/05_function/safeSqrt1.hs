safeSqrt x
	| x >= 0 = Just (sqrt x)
	| otherwise = Nothing
