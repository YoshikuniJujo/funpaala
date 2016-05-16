import Funpaala

ffldr :: (a -> b -> b) -> [a] -> b -> b
-- ffldr op = flip $ fldr op
ffldr = flip . fldr
