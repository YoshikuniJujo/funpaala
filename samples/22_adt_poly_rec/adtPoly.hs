data Twice a = Twice a a deriving Show

mapTwice :: (a -> b) -> Twice a -> Twice b
mapTwice f (Twice x y) = Twice (f x) (f y)

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

data MyMaybe a = MyJust a | MyNothing deriving Show

myFromMaybe :: a -> MyMaybe a -> a
myFromMaybe _ (MyJust x) = x
myFromMaybe d _ = d

myMaybe :: b -> (a -> b) -> MyMaybe a -> b
myMaybe _ f (MyJust x) = f x
myMaybe d _ _ = d

data MyEither a b = MyLeft a | MyRight b deriving Show

myEither :: (a -> c) -> (b -> c) -> MyEither a b -> c
myEither f _ (MyLeft x) = f x
myEither _ g (MyRight y) = g y
