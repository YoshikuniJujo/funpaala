data Twin a = Twin a a deriving Show

mapTwin :: (a -> b) -> Twin a -> Twin b
mapTwin f (Twin x y) = Twin (f x) (f y)

data Rep a = Rep Int a deriving Show

toList :: Rep a -> [a]
toList (Rep n x) = replicate n x

data Option a b = Single a | Option a b deriving Show

human :: Option String Int -> String
human (Single n) = n
human (Option n a) = n ++ " (" ++ show a ++ ")"

data Tuple a b = Tuple a b deriving Show

fstT :: Tuple a b -> a
fstT (Tuple x _) = x

sndT :: Tuple a b -> b
sndT (Tuple _ y) = y

toTuple :: (a, b) -> Tuple a b
toTuple (x, y) = Tuple x y

fromTuple :: Tuple a b -> (a, b)
fromTuple (Tuple x y) = (x, y)

data MyMaybe a = MyJust a | MyNothing deriving Show

myFromMaybe :: a -> MyMaybe a -> a
myFromMaybe _ (MyJust x) = x
myFromMaybe d MyNothing = d

myMaybe :: b -> (a -> b) -> MyMaybe a -> b
myMaybe _ f (MyJust x) = f x
myMaybe d _ MyNothing = d

data MyEither a b = MyLeft a | MyRight b deriving Show

myEither :: (a -> c) -> (b -> c) -> MyEither a b -> c
myEither f _ (MyLeft x) = f x
myEither _ g (MyRight y) = g y

data List a = Nil | a :~ List a deriving Show

mapL :: (a -> b) -> List a -> List b
mapL f (x :~ xs) = f x :~ mapL f xs
mapL _ Nil = Nil
