import Data.Bool (bool)

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = concat . map f

filterC :: (a -> Bool) -> [a] -> [a]
filterC p = concatMap $ \x -> bool [] [x] (p x)
