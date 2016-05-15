import Text.Read

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish :: [String] -> Maybe [Integer]
polish [] = Just []
polish (s : ss) = case lookup s operators of
	Just o -> case polish ss of
		Just (x : y : ns) -> Just $ x `o` y : ns
		_ -> Nothing
	_ -> case readMaybe s of
		Just n -> maybe Nothing (Just . (n :)) $ polish ss
		_ -> Nothing
