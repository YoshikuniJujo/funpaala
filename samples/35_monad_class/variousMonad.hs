{-# LANGUAGE TupleSections #-}

safeDivM :: Int -> Int -> Maybe Int
_ `safeDivM` 0 = Nothing
x `safeDivM` y = Just $ x `div` y

calcM :: Int -> Int -> Int -> Maybe Int
calcM a b c =
	a `safeDivM` b >>= \x ->
	x `safeDivM` c

data Try a = Error String | Success a deriving Show

instance Functor Try where
	fmap = (=<<) . (return .)

instance Applicative Try where
	pure = return
	mf <*> mx =
		mf >>= \f ->
		mx >>= \x ->
		return $ f x

instance Monad Try where
	return = Success
	Error em >>= _ = Error em
	Success x >>= f = f x

safeDivT :: Int -> Int -> Try Int
x `safeDivT` 0 = Error $ show x ++ " is divided by zero\n"
x `safeDivT` y = Success $ x `div` y

calcT :: Int -> Int -> Int -> Try Int
calcT a b c =
	a `safeDivT` b >>= \x ->
	x `safeDivT` c

newtype Calc a = Calc { runCalc :: Int -> (a, Int) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s ->
		let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Int -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Int
mrecall = Calc $ \s -> (s, s)

example :: Calc Int
{-
example =
	return (3 * 4) >>=
	mplus >>= \_ ->
	return (2 * 5) >>=
	mplus >>= \_ ->
	mrecall >>= \x ->
	return (x * 7)
-}
{-
example =
	return (3 * 4) >>=
	mplus >>
	return (2 * 5) >>=
	mplus >>
	mrecall >>=
	return . (* 7)
-}
{-
example = do
	x <- return (3 * 4)
	mplus x
	y <- return (2 * 5)
	mplus y
	z <- mrecall
	return (z * 7)
-}
example = do
	let x = 3 * 4
	mplus x
	let y = 2 * 5
	mplus y
	z <- mrecall
	return (z * 7)
