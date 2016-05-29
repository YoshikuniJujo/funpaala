{-# LANGUAGE MonadComprehensions #-}

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap = (=<<) . (return .)

instance Applicative (State s) where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (State s) where
	return = State . (,)
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

madd :: Integer -> State Integer ()
madd x = modify (+ x)

example :: State Integer Integer
example = do
	madd $ 3 * 4
	madd $ 2 * 5
	(* 7) <$> get
