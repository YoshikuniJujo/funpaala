madd :: Int -> Int -> (Int, Int)
madd x m = (x, x + m)

mrecall :: a -> Int -> (Int, Int)
mrecall _ m = (m, m)

-- arrC :: (Int -> Int) -> Int -> Int -> (Int, Int)
arrC :: (a -> b) -> a -> Int -> (b, Int)
arrC f x m = (f x, m)

type Calc a b = a -> Int -> (b, Int)

pipeC :: Calc a b -> Calc b c -> Calc a c
(f `pipeC` g) x m = let (x', m') = f x m in g x' m'

{-
example :: Calc () Int
example =
	arrC (const 3) `pipeC`
	arrC (* 4) `pipeC`
	madd `pipeC`
	arrC (const 2) `pipeC`
	arrC (* 5) `pipeC`
	madd `pipeC`
	mrecall `pipeC`
	arrC (* 7)
-}

type State a = Int -> (a, Int)

bindC :: State a -> (a -> State b) -> State b
(f `bindC` g) m = let (x, m') = f m in g x m'

retC :: a -> State a
retC x m = (x, m)

{-
example :: State Int
example =
	retC 3 `bindC`
	(retC . (* 4)) `bindC`
	madd `bindC`
	const (retC 2) `bindC`
	(retC . (* 5)) `bindC`
	madd `bindC`
	mrecall `bindC`
	(retC . (* 7))
-}

example :: State Int
example =
	retC 3 `bindC` \x ->
	retC (x * 4) `bindC` \y ->
	madd y `bindC` \_ ->
	retC 2 `bindC` \z ->
	retC (z * 5) `bindC` \w ->
	madd w `bindC` \_ ->
	mrecall () `bindC` \v ->
	retC (v * 7)
