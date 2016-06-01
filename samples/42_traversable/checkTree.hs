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

sampleTree :: STree Int
sampleTree = Node
	(Node
		(Node Tip 2 Tip)
		3
		(Node Tip 5 Tip))
	8
	(Node
		(Node Tip 7 Tip)
		5
		(Node Tip 15 Tip))

check1 :: Ord a => a -> State a (a, Bool)
check1 x = do
	s <- get
	put $ x `max` s
	return (x, x >= s)

checkTreeS :: Ord a => STree a -> State a (STree (a, Bool))
checkTreeS (Node l x r) = Node <$> checkTreeS l <*> check1 x <*> checkTreeS r
checkTreeS _ = pure Tip

checkTree :: (Bounded a, Ord a) => STree a -> STree (a, Bool)
checkTree = (`evalState` minBound) . checkTreeS

ttraverse :: Applicative f => (a -> f b) -> STree a -> f (STree b)
ttraverse f (Node l x r) = Node <$> ttraverse f l <*> f x <*> ttraverse f r
ttraverse _ _ = pure Tip

checkTree' :: (Bounded a, Ord a) => STree a -> STree (a, Bool)
checkTree' = (`evalState` minBound) . ttraverse check1

ltraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
ltraverse f (x : xs) = (:) <$> f x <*> ltraverse f xs
ltraverse _ _ = pure []

checkList :: (Bounded a, Ord a) => [a] -> [(a, Bool)]
checkList = (`evalState` minBound) . ltraverse check1

instance Functor STree where
	fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)
	fmap _ _ = Tip

instance Foldable STree where
	foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r
	foldMap _ _ = mempty

instance Traversable STree where
	traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r
	traverse _ _ = pure Tip

check :: (Traversable t, Bounded a, Ord a) => t a -> t (a, Bool)
check = (`evalState` minBound) . traverse check1
