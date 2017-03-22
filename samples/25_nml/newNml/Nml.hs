module Nml(Nml, nml, fromNml) where

import Data.List
import Data.Tree
import Data.Char

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
parseL ts = flip (maybe ([], ts)) (parse ts) $ \(n, r) ->
	let (ns, r') = parseL r in (n : ns, r')

nml :: String -> Maybe Nml
nml = maybe Nothing (Just . fst) . parse . unfoldr token

fromNml :: Nml -> String
fromNml = toString 0 . toTokens

idt :: Int -> String -> String
idt i = (replicate i '\t' ++) . (++ "\n")

opn, cls :: String -> String
opn = ('<' :) . (++ ">")
cls = ("</" ++) . (++ ">")

toString :: Int -> [Token] -> String
toString i (Open o : Text tx : Close c : ts) =
	idt i (opn o ++ tx ++ cls c) ++ toString i ts
toString i (Open o : ts) = idt i (opn o) ++ toString (i + 1) ts
toString i (Close c : ts) = idt (i - 1) (cls c) ++ toString (i - 1) ts
toString i (Text tx : ts) = idt i tx ++ toString i ts
toString _ [] = ""

toTokens :: Nml -> [Token]
toTokens (Node tx []) = [Text tx]
toTokens (Node tg ns) = Open tg : concatMap toTokens ns ++ [Close tg]

sample1 :: Nml
sample1 = Node "hello" [Node "world" []]
