{-# LANGUAGE MonadComprehensions, TupleSections #-}

safeDiv :: Integer -> Integer -> Maybe Integer
_ `safeDiv` 0 = Nothing
x `safeDiv` y = Just $ x `div` y

calc :: Integer -> Integer -> Integer -> Maybe Integer
calc a b c = [ y | x <- a `safeDiv` b, y <- x `safeDiv` c ]
