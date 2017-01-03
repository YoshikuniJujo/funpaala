import Prelude hiding (fst, snd)

data C a = C { (.$.) :: C a -> C a } | V { value :: a } | Nil

instance Show a => Show (C a) where
	show (C _) = "<function>"
	show (V x) = "V " ++ show x
	show Nil = "Nil"

instance Num a => Num (C a) where
	V x + V y = V $ x + y
	V x * V y = V $ x * y
	negate = V . negate . value
	abs = V . abs . value
	signum = V . signum . value
	fromInteger = V . fromInteger

pair :: C a -> C a -> C a
pair x y = C $ \f -> f .$. x .$. y

fst, snd :: C a -> C a
fst p = p .$. C (\x -> C (\_ -> x))
snd p = p .$. C (\_ -> C (\y -> y))
