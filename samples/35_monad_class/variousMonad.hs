{-# LANGUAGE TupleSections #-}

safeDivM :: Integer -> Integer -> Maybe Integer
_ `safeDivM` 0 = Nothing
x `safeDivM` y = Just $ x `div` y

calcM :: Integer -> Integer -> Integer -> Maybe Integer
calcM a b c =
	a `safeDivM` b >>= \x ->
	x `safeDivM` c

data Try a = Error String | Success a deriving Show

retT :: a -> Try a
retT = Success

bindT :: Try a -> (a -> Try b) -> Try b
Error em `bindT` _ = Error em
Success x `bindT` f = f x

instance Functor Try where
	fmap f m = m `bindT` (retT . f)

instance Applicative Try where
	pure = retT
	mf <*> mx =
		mf `bindT` \f ->
		mx `bindT` \x ->
		retT $ f x

instance Monad Try where
	return = retT
	(>>=) = bindT

safeDivT :: Integer -> Integer -> Try Integer
x `safeDivT` 0 = Error $ show x ++ " is divided by zero\n"
x `safeDivT` y = Success $ x `div` y

calcT :: Integer -> Integer -> Integer -> Try Integer
calcT a b c =
	a `safeDivT` b >>= \x ->
	x `safeDivT` c

newtype State a = State { runState :: Integer -> (a, Integer) }

retS :: a -> State a
retS x = State $ \s -> (x, s)

bindS :: State a -> (a -> State b) -> State b
m `bindS` f = State $ \s -> let (x, s') = runState m s in runState (f x) s'

instance Functor State where
	fmap f m = m `bindS` (retS . f)

instance Applicative State where
	pure = retS
	mf <*> mx = mf `bindS` \f -> mx `bindS` \x -> retS $ f x

instance Monad State where
	return = retS
	(>>=) = bindS

mplus :: Integer -> State ()
mplus x = State $ (() ,) . (+ x)

mrecall :: State Integer
mrecall = State $ \s -> (s, s)

example :: State Integer
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
