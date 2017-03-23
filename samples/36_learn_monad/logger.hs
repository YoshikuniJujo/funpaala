import Data.Char

data Logger a = Logger [String] a deriving Show

toCode :: Char -> Logger Int
-- toCode c = Logger ["toCode " ++ show c] (ord c)
-- toCode c = tell ("toCode " ++ show c) >> return (ord c)
toCode c = ord c <$ tell ("toCode " ++ show c)

double :: Int -> Logger Int
-- double n = Logger ["double " ++ show n] (n * 2)
-- double n = tell ("double " ++ show n) >> return (n * 2)
double n = n * 2 <$ tell ("double " ++ show n)

retL :: a -> Logger a
retL = Logger []

bindL :: Logger a -> (a -> Logger b) -> Logger b
Logger l x `bindL` f = let Logger l' y = f x in Logger (l ++ l') y

instance Functor Logger where
	fmap f m = m `bindL` (retL . f)

instance Applicative Logger where
	pure = retL
	mf <*> mx = mf `bindL` \f -> mx `bindL` \x -> retL $ f x

instance Monad Logger where
	return = retL
	(>>=) = bindL

tell :: String -> Logger ()
tell l = Logger [l] ()
