import Data.Bool

type Questions a = [(a, a)]
type Answers = [Bool]
type Results a = [(Bool, (a, a))]

questions1 :: Questions String
questions1 = [
	("Elephant", "Giraffe"),
	("Apple", "Banana"),
	("TV", "Radio"),
	("Plane", "Ship"),
	("Tennis", "Ski") ]

answers1 :: Answers
answers1 = [True, False, True, True, False]

result :: Questions a -> Answers -> Results a
result = flip zip

likes, dislikes :: Results a -> [a]
likes = map $ \(a, s) -> (if a then fst else snd) s
dislikes = map $ \(a, s) -> (if a then snd else fst) s

questions2 :: Questions (Int, String)
questions2 = [
	((112, "Chair"), (113, "Sofa")),
	((343, "Telephone"), (344, "Fax")),
	((522, "Projector"), (523, "Monitor")) ]

answers2 :: Answers
answers2 = [False, True, True]
