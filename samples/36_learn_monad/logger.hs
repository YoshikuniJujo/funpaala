{-# LANGUAGE MonadComprehensions #-}

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

instance Functor Logger where
	fmap = (=<<) . (return .)

instance Applicative Logger where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad Logger where
	return = Logger []
	Logger l x >>= f = let Logger l' y = f x in Logger (l ++ l') y

tell :: String -> Logger ()
tell l = Logger [l] ()
