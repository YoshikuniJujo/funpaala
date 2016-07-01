newtype State a = State { runState :: Integer -> (a, Integer) }

retS :: a -> State a
retS x = State $ \s -> (x, s)

bindS :: State a -> (a -> State b) -> State b
State m `bindS` f = State $ \s -> let (x, s') = m s in runState (f x) s'

instance Functor State where
	fmap f m = m `bindS` (retS . f)

instance Applicative State where
	pure = retS
	mf <*> mx = mf `bindS` \f -> mx `bindS` \x -> retS $ f x

instance Monad State where
	return = retS
	(>>=) = bindS

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
