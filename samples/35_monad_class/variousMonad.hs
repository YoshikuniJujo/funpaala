{-# LANGUAGE TupleSections #-}

safeDivM :: Integer -> Integer -> Maybe Integer
_ `safeDivM` 0 = Nothing
x `safeDivM` y = Just $ x `div` y

calcM :: Integer -> Integer -> Integer -> Maybe Integer
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

safeDivT :: Integer -> Integer -> Try Integer
x `safeDivT` 0 = Error $ show x ++ " is divided by zero\n"
x `safeDivT` y = Success $ x `div` y

calcT :: Integer -> Integer -> Integer -> Try Integer
calcT a b c =
	a `safeDivT` b >>= \x ->
	x `safeDivT` c

newtype Calc a = Calc { runCalc :: Integer -> (a, Integer) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	mf <*> mx = mf >>= \f -> mx >>= \x -> return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s ->
		let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Integer -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Integer
mrecall = Calc $ \s -> (s, s)

example :: Calc Integer
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
