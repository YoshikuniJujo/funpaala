module Nml(Nml, nml, fromNml) where

import Data.List (unfoldr)
import Data.Tree (Tree(..))
import Data.Char (isSpace)

data Token = Open String | Close String | Text String deriving Show

token :: String -> Maybe (Token, String)
token "" = Nothing
token ('<' : '/' : s) = tag Close s
token ('<' : s) = tag Open s
token s	| all isSpace tx = token r
	| otherwise = Just (Text tx, r)
	where (tx, r) = span (/= '<') s

tag :: (String -> Token) -> String -> Maybe (Token, String)
tag f s = case span (/= '>') s of
	(tg, _ : r) -> Just (f tg, r)
	_ -> Nothing

type Nml = Tree String

parse :: [Token] -> Maybe (Nml, [Token])
parse (Open o : ts) = case parseL ts of
	(ns, Close c : r) | o == c -> Just (Node o ns, r)
	_ -> Nothing
parse (Text tx : ts) = Just (Node tx [], ts)
parse _ = Nothing

parseL :: [Token] -> ([Nml], [Token])
parseL ts = case parse ts of
	Nothing -> ([], ts)
	Just (n, r) -> (n : ns, r')
		where (ns, r') = parseL r

nml :: String -> Maybe Nml
nml = fmap fst . parse . unfoldr token

fromNml :: Nml -> String
fromNml = concatMap toString . toTokens

toString :: Token -> String
toString (Open o) = "<" ++ o ++ ">"
toString (Close c) = "</" ++ c ++ ">"
toString (Text tx) = tx

toTokens :: Nml -> [Token]
toTokens (Node tx []) = [Text tx]
toTokens (Node tg ns) = Open tg : concatMap toTokens ns ++ [Close tg]

sample1 :: Nml
sample1 = Node "hello" [Node "world" []]
