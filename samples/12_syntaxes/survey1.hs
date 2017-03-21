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
likes = map . uncurry $ bool snd fst
dislikes = map . uncurry $ bool fst snd
