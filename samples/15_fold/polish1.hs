import Text.Read
import Funpaala

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish1 :: String -> Maybe [Integer] -> Maybe [Integer]
polish1 s (Just ns) = case lookup s operators of
	Just o -> case ns of
		x : y : ns' -> Just $ x `o` y : ns'
		_ -> Nothing
	_ -> fmap (: ns) $ readMaybe s
polish1 _ _ = Nothing

polish :: [String] -> Maybe [Integer]
polish = fldr polish1 (Just [])
