import Data.Tree (Tree(..))
import Data.Char (ord)

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f (Just x) = Just $ f x
mmap _ Nothing = Nothing

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Node x sf) = Node (f x) $ map (tmap f) sf

toCode :: Functor f => f Char -> f Int
toCode = fmap ord

newtype Fun a = Fun { fun :: Integer -> a }

instance Functor Fun where
	fmap f (Fun g) = Fun $ \x -> f $ g x

brandBag :: Integer -> Integer
brandBag = \t -> 50000 * (100 + t) `div` 100

iWantFourBag :: Integer -> Integer
iWantFourBag = fmap (* 4) brandBag
