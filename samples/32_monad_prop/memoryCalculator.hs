-- madd :: Integer -> Integer -> (Integer, Integer)
madd :: Calc Integer Integer
madd x m = (x, x + m)

-- mrecall :: Integer -> Integer -> (Integer, Integer)
-- mrecall :: a -> Integer -> (Integer, Integer)
mrecall :: Calc a Integer
mrecall _ m = (m, m)

-- arrC :: (Integer -> Integer) -> Integer -> Integer -> (Integer, Integer)
-- arrC :: (a -> b) -> a -> Integer -> (b, Integer)
arrC :: (a -> b) -> Calc a b
arrC f x m = (f x, m)

type Calc a b = a -> Integer -> (b, Integer)

pipeC :: Calc a b -> Calc b c -> Calc a c
(f `pipeC` g) x m = let (x', m') = f x m in g x' m'

example :: Calc Integer Integer
example =
	arrC (const 3) `pipeC`
	arrC (* 4) `pipeC`
	madd `pipeC`
	arrC (const 2) `pipeC`
	arrC (* 5) `pipeC`
	madd `pipeC`
	mrecall `pipeC`
	arrC (* 7)

type State a = Integer -> (a, Integer)

bindS :: State a -> (a -> State b) -> State b
(f `bindS` g) m = let (x, m') = f m in g x m'

retS :: a -> State a
retS x m = (x, m)

example' :: State Integer
example' =
	retS 3 `bindS`
	(retS . (* 4)) `bindS`
	madd `bindS`
	const (retS 2) `bindS`
	(retS . (* 5)) `bindS`
	madd `bindS`
	mrecall `bindS`
	(retS . (* 7))

example'' :: State Integer
example'' =
	retS 3 `bindS` \x ->
	retS (x * 4) `bindS` \y ->
	madd y `bindS` \_ ->
	retS 2 `bindS` \z ->
	retS (z * 5) `bindS` \w ->
	madd w `bindS` \_ ->
	mrecall () `bindS` \v ->
	retS (v * 7)
