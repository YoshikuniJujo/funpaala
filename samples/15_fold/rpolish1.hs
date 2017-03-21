import Text.Read
import Funpaala

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
	Just o -> case ns of
		y : x : ns' -> Just $ x `o` y : ns'
		_ -> Nothing
	Nothing -> fmap (: ns) $ readMaybe s
rpolish1 Nothing _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = fldl' rpolish1 $ Just []
