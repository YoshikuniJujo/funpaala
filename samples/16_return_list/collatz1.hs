import Data.Bool (bool)

takeTo :: (a -> Bool) -> [a] -> [a]
takeTo p = foldr (\x -> (x :) . bool id (const []) (p x)) []

collatzInf :: Integer -> [Integer]
-- collatzInf = iterate $ \n -> if even n then n `div` 2 else n * 3 + 1
collatzInf = iterate $ \n -> bool (n * 3 + 1) (n `div` 2) (even n)

collatz :: Integer -> [Integer]
collatz = takeTo (== 1) . collatzInf
