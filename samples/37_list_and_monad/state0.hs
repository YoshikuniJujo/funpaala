{-# LANGUAGE MonadComprehensions #-}

newtype State a = State { runState :: Integer -> (a, Integer) }

instance Functor State where
	fmap = (=<<) . (return .)

instance Applicative State where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad State where
	return = State . (,)
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'

get :: State Integer
get = State $ \s -> (s, s)

put :: Integer -> State ()
put x = State $ \_ -> ((), x)

modify :: (Integer -> Integer) -> State ()
modify f = get >>= put . f

madd :: Integer -> State ()
madd x = modify (+ x)

example :: State Integer
example = do
	madd $ 3 * 4
	madd $ 2 * 5
	(* 7) <$> get
