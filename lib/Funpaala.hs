module Funpaala (funpaala, fldr, fldl, fldl') where

funpaala :: IO ()
funpaala = putStrLn "FUNctional Programming As A Liberal Arts"

fldr :: (a -> b -> b) -> b -> [a] -> b
fldr op v (x : xs) = x `op` fldr op v xs
fldr _ v _ = v

fldl, fldl' :: (a -> b -> a) -> a -> [b] -> a
fldl op s (x : xs) = fldl op (s `op` x) xs
fldl _ s _ = s

fldl' op s (x : xs) = s `seq` fldl' op (s `op` x) xs
fldl' _ s _ = s
