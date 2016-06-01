data Item = Banana | Club | Shield | Armor | Sord | Star
	deriving (Show, Eq, Ord)

type Bagage = [Item]

data Stage = Fork Stage Stage | Goal Item deriving Show

point :: Item -> Integer
point Banana = 10
point Club = 20
point Shield = 50
point Armor = 80
point Sord = 100
point Star = 200

bagage1, bagage2, bagage3 :: Bagage
bagage1 = [Banana, Armor, Star, Sord]
bagage2 = [Club, Armor, Shield, Star, Star]
bagage3 = [Shield, Sord, Star]

stage1, stage2, stage3 :: Stage
stage1 = Fork
	(Fork (Goal Club) (Goal Sord))
	(Fork (Goal Shield) (Goal Club))

stage2 = Fork
	(Fork
		(Goal Banana)
		(Fork
			(Goal Shield)
			(Goal Banana)))
	(Fork
		(Goal Star)
		(Goal Sord))

stage3 = Fork
	(Goal Shield)
	(Fork
		(Fork
			(Goal Armor)
			(Goal Shield))
		(Goal Shield))

yourItems :: Bagage -> Integer
yourItems (_ : is) = 1 + yourItems is
yourItems _ = 0

yourPoint :: Bagage -> Integer
yourPoint (i : is) = point i + yourPoint is
yourPoint _ = 100

yours :: (Item -> b -> b) -> b -> Bagage -> b
yours op v (i : is) = i `op` yours op v is
yours op v _ = v

fyours :: (Item -> (b -> b)) -> Bagage -> (b -> b)
fyours op (i : is) = op i . fyours op is
fyours _ _ = id

doesStarExist :: Stage -> Bool -> Bool
doesStarExist (Fork l r) = doesStarExist l . doesStarExist r
doesStarExist (Goal Star) = const True
doesStarExist _ = id

stageItems :: Stage -> Integer -> Integer
stageItems (Fork l r) = stageItems l . stageItems r
stageItems _ = (1 +)

stagePoint :: Stage -> Integer -> Integer
stagePoint (Fork l r) = stagePoint l . stagePoint r
stagePoint (Goal i) = (point i +)

bestItem :: Stage -> Item -> Item
bestItem (Fork l r) = bestItem l . bestItem r
bestItem (Goal i) = (i `max`)

worstItem :: Stage -> Item -> Item
worstItem (Fork l r) = worstItem l . worstItem r
worstItem (Goal i) = (i `min`)

stages :: (Item -> b -> b) -> Stage -> b -> b
stages op (Fork l r) = stages op l . stages op r
stages op (Goal i) = (i `op`)

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show
type Stage' = BinTree Item

tffoldr :: (a -> b -> b) -> BinTree a -> b -> b
tffoldr op (Node l r) = tffoldr op l . tffoldr op r
tffoldr op (Leaf x) = (x `op`)

instance Foldable BinTree where
	foldMap f (Node l r) = foldMap f l `mappend` foldMap f r
	foldMap f (Leaf x) = f x

btStage1 :: Stage'
btStage1 = Node
	(Node
		(Leaf Banana)
		(Node
			(Leaf Shield)
			(Leaf Banana)))
	(Node
		(Leaf Star)
		(Leaf Sord))

yourItems' :: Bagage -> Integer
yourItems' = fromIntegral . length

stageItems' :: Stage' -> Integer
stageItems' = fromIntegral . length

yourPoint' :: Bagage -> Integer
yourPoint' = foldr ((+) . point) 100

stagePoint' :: Stage' -> Integer
stagePoint' = foldr ((+) . point) 0

doesStarExist' :: Stage' -> Bool
doesStarExist' = (Star `elem`)

bestItem' :: Stage' -> Item
bestItem' = maximum

worstItem' :: Stage' -> Item
worstItem' = minimum
