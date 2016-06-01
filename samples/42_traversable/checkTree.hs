{-# LANGUAGE MonadComprehensions #-}

data State s a = State { runState :: s -> (a, s) }

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

evalState :: State s a -> s -> a
evalState = (fst .) . runState

data STree a = Tip | Node (STree a) a (STree a) deriving Show

check1 :: Ord a => a -> State a (a, Bool)
check1 x = do
	s <- get
	put $ x `max` s
	return (x, x >= s)
