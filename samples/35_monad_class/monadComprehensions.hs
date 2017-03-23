{-# LANGUAGE MonadComprehensions #-}

safeDiv :: Integer -> Integer -> Maybe Integer
_ `safeDiv` 0 = Nothing
x `safeDiv` y = Just $ x `div` y

calc :: Integer -> Integer -> Integer -> Maybe Integer
calc a b c = [ y | x <- a `safeDiv` b, y <- x `safeDiv` c ]

newtype State a = State { runState :: Integer -> (a, Integer) }

retS :: a -> State a
retS x = State $ \s -> (x, s)

bindS :: State a -> (a -> State b) -> State b
m `bindS` f = State $ \s -> let (x, s') = runState m s in runState (f x) s'

instance Functor State where
	fmap f = (`bindS` retS . f)

instance Applicative State where
	pure = retS
	mf <*> mx = mf `bindS` \f -> mx `bindS` \x -> retS $ f x

instance Monad State where
	return = retS
	(>>=) = bindS

madd :: Integer -> State ()
madd x = State $ \s -> ((), s + x)

mrecall :: State Integer
mrecall = State $ \s -> (s, s)

calcC :: State Integer
calcC = [ x * 7 | _ <- madd $ 3 * 4, _ <- madd $ 2 * 5, x <- mrecall ]
