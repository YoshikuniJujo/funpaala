{-# LANGUAGE MonadComprehensions, TupleSections #-}

safeDiv :: Integer -> Integer -> Maybe Integer
_ `safeDiv` 0 = Nothing
x `safeDiv` y = Just $ x `div` y

calc :: Integer -> Integer -> Integer -> Maybe Integer
calc a b c = [ y | x <- a `safeDiv` b, y <- x `safeDiv` c ]

newtype Calc a = Calc { runCalc :: Int -> (a, Int) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	mf <*> mx = do f <- mf; x <- mx; return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s ->
		let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Int -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Int
mrecall = Calc $ \s -> (s, s)

calcC :: Calc Int
calcC = [ x * 7 | _ <- mplus $ 3 * 4, _ <- mplus $ 2 * 5, x <- mrecall ]
