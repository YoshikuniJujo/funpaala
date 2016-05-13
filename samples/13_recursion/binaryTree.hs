type Tree = [(Char, (Char, Char))]

sampleTree :: Tree
sampleTree = [('a', ('b', 'c')), ('b', ('d', 'e')), ('e', ('f', 'g'))]

reachable :: Tree -> Char -> Char -> Bool
reachable _ s e | s == e = True
reachable t s e = case lookup s t of
	Nothing -> False
	Just (l, r) -> reachable t l e || reachable t r e
