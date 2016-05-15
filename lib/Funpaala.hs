module Funpaala (funpaala, fldr) where

funpaala :: IO ()
funpaala = putStrLn "FUNctional Programming As A Liberal Arts"

fldr :: (a -> b -> b) -> b -> [a] -> b
fldr op v (x : xs) = x `op` fldr op v xs
fldr _ v _ = v
