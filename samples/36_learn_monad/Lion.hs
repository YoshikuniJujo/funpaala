module Lion (Lion, Caged, lion, feed, play) where

data Lion = Lion Name State deriving Show

type Name = String
data State = Hungry | Normal | Full deriving Show

newtype Caged a = Caged a deriving Show

retC :: a -> Caged a
retC = Caged

bindC :: Caged a -> (a -> Caged b) -> Caged b
Caged x `bindC` f = f x

instance Functor Caged where
	fmap f m = m `bindC` (retC . f)

instance Applicative Caged where
	pure = retC
	mf <*> mx = mf `bindC` \f -> mx `bindC` \x -> retC $ f x

instance Monad Caged where
	return = retC
	(>>=) = bindC

lion :: Name -> Caged Lion
lion n = Caged $ Lion n Hungry

feed, play :: Lion -> Lion
feed (Lion n Hungry) = Lion n Normal
feed (Lion n _) = Lion n Full

play (Lion n Full) = Lion n Normal
play (Lion n _) = Lion n Hungry
