import Data.Monoid (Sum(..), Product(..), Any(..), Endo(..))

lelem :: Eq a => a -> [a] -> Bool
lelem x (y : ys) = y `op` lelem x ys
	where op = (||) . (== x)
lelem _ [] = False

lsum :: Num a => [a] -> a
lsum (x : xs) = x + lsum xs
lsum [] = 0

lproduct :: Num a => [a] -> a
lproduct (x : xs) = x * lproduct xs
lproduct [] = 1

lfoldr op v (x : xs) = x `op` lfoldr op v xs
lfoldr _ v [] = v

lfoldMap :: Monoid m => (a -> m) -> [a] -> m
lfoldMap f (x : xs) = f x `mappend` lfoldMap f xs
lfoldMap _ [] = mempty

lelem' :: Eq a => a -> [a] -> Bool
lelem' x = getAny . lfoldMap (Any . (== x))

lsum', lproduct' :: Num a => [a] -> a
lsum' = getSum . lfoldMap Sum
lproduct' = getProduct . lfoldMap Product

lfoldMap' :: Monoid m => (a -> m) -> [a] -> m
lfoldMap' f = lfoldr (mappend . f) mempty

lffoldr :: (a -> b -> b) -> [a] -> b -> b
lffoldr op = appEndo . lfoldMap (Endo . op)

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

tree1 :: BinTree Integer
tree1 = Node
	(Node
		(Leaf 2)
		(Node (Leaf 3) (Leaf 9)))
	(Node
		(Node (Leaf 8) (Leaf 7))
		(Node (Leaf 5) (Leaf 12)))

telem :: Eq a => a -> BinTree a -> Bool -> Bool
telem x (Node l r) = telem x l . telem x r
telem x (Leaf y) = (x == y ||)

tsum :: Num a => BinTree a -> a -> a
tsum (Node l r) = tsum l . tsum r
tsum (Leaf x) = (x +)

tproduct :: Num a => BinTree a -> a -> a
tproduct (Node l r) = tproduct l . tproduct r
tproduct (Leaf x) = (x *)

tffoldr :: (a -> b -> b) -> BinTree a -> b -> b
tffoldr op (Node l r) = tffoldr op l . tffoldr op r
tffoldr op (Leaf x) = (x `op`)

tfoldr :: (a -> b -> b) -> b -> BinTree a -> b
tfoldr op v (Node l r) = tfoldr op (tfoldr op v r) l
tfoldr op v (Leaf x) = x `op` v

tfoldMap :: Monoid m => (a -> m) -> BinTree a -> m
tfoldMap f (Node l r) = tfoldMap f l `mappend` tfoldMap f r
tfoldMap f (Leaf x) = f x

tfoldMap' :: Monoid m => (a -> m) -> BinTree a -> m
tfoldMap' f = tfoldr (mappend . f) mempty

tffoldr' :: (a -> b -> b) -> BinTree a -> b -> b
tffoldr' op = appEndo . tfoldMap (Endo . op)
