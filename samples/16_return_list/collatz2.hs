import Data.Bool
import Funpaala

takeTo :: (a -> Bool) -> [a] -> [a]
takeTo p = fldr (\x lst -> bool (x : lst) [x] (p x)) []

collatzInf :: Integer -> [Integer]
collatzInf = iterate $ \n -> if even n then n `div` 2 else n * 3 + 1

collatz :: Integer -> [Integer]
collatz = takeTo (== 1) . collatzInf
