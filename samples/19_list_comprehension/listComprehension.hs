import Data.Bool

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap = (concat .) . map

filterC :: (a -> Bool) -> [a] -> [a]
filterC p = concatMap $ \x -> bool [] [x] (p x)
