class Composable a where
	compose :: a -> a -> a

data Add = Add Integer deriving Show

instance Composable Add where
	compose (Add x) (Add y) = Add $ x + y

data Mul = Mul Integer deriving Show

instance Composable Mul where
	compose (Mul x) (Mul y) = Mul $ x * y
