import Funpaala

ffldr :: (a -> b -> b) -> [a] -> b -> b
ffldr op (x : xs) v = x `op` ffldr op xs v
ffldr _ _ v = v
