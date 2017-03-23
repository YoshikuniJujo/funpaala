bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
Just x `bindM` f = f x
Nothing `bindM` _ = Nothing

retM :: a -> Maybe a
retM = Just

appM :: Maybe (a -> b) -> Maybe a -> Maybe b
Just f `appM` Just x = Just $ f x
_ `appM` _ = Nothing

pureM :: a -> Maybe a
pureM = Just

fappM :: Maybe a -> Maybe (a -> b) -> Maybe b
fappM = flip appM

safeDivM :: Integer -> Integer -> Maybe Integer
safeDivM _ 0 = Nothing
safeDivM x y = Just $ x `div` y

calcM :: Integer -> Integer -> Integer -> Integer -> Maybe Integer
calcM a b c d =
	a `safeDivM` b `bindM` \x ->
	c `safeDivM` d `bindM` \y ->
	retM $ x + y

data TryM a = ErrorM String | SuccessM a deriving Show

retT :: a -> TryM a
retT = SuccessM

bindT :: TryM a -> (a -> TryM b) -> TryM b
SuccessM x `bindT` f = f x
ErrorM em `bindT` _ = ErrorM em

safeDivTM :: Integer -> Integer -> TryM Integer
safeDivTM x 0 = ErrorM $ show x ++ " is divided by zero\n"
safeDivTM x y = SuccessM $ x `div` y

mappT :: TryM (a -> b) -> TryM a -> TryM b
tf `mappT` tx = tf `bindT` \f -> tx `bindT` \x -> retT $ f x

calcTM :: Integer -> Integer -> Integer -> Integer -> TryM Integer
calcTM a b c d = retT (+) `mappT` (a `safeDivTM` b) `mappT` (c `safeDivTM` d)

data TryA a = ErrorA String | SuccessA a deriving Show

pureT :: a -> TryA a
pureT = SuccessA

appT :: TryA (a -> b) -> TryA a -> TryA b
SuccessA f `appT` SuccessA x = SuccessA $ f x
SuccessA _ `appT` ErrorA em' = ErrorA em'
ErrorA em `appT` SuccessA _ = ErrorA em
ErrorA em `appT` ErrorA em' = ErrorA $ em ++ em'

safeDivTA :: Integer -> Integer -> TryA Integer
safeDivTA x 0 = ErrorA $ show x ++ " is divided by zero\n"
safeDivTA x y = SuccessA $ x `div` y

calcTA :: Integer -> Integer -> Integer -> Integer -> TryA Integer
calcTA a b c d = pureT (+) `appT` (a `safeDivTA` b) `appT` (c `safeDivTA` d)

calc2 :: Integer -> Integer -> Integer -> TryM Integer
calc2 a b c =
	a `safeDivTM` b `bindT` \x ->
	x `safeDivTM` c
