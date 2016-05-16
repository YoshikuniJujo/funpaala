import Data.List
import Text.Read
import Funpaala

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

rpolishIter :: Maybe [Integer] -> [String] -> Maybe [Integer]
rpolishIter mns [] = mns
rpolishIter (Just ns) (s : ss) = case lookup s operators of
	Just o -> case ns of
		y : x : ns' -> rpolishIter (Just $ x `o` y : ns') ss
		_ -> Nothing
	_ -> rpolishIter (maybe Nothing (Just . (: ns)) $ readMaybe s) ss
rpolishIter _ _ = Nothing

rpolish :: [String] -> Maybe [Integer]
rpolish = fldl' rpolish1 $ Just []

rpolish1 :: Maybe [Integer] -> String -> Maybe [Integer]
rpolish1 (Just ns) s = case lookup s operators of
	Just o -> case ns of
		y : x : ns' -> Just $ x `o` y : ns'
		_ -> Nothing
	_ -> maybe Nothing (Just . (: ns)) $ readMaybe s
rpolish1 _ _ = Nothing
