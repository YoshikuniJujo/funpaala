{-# LANGUAGE MonadComprehensions #-}

import Data.Bool

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
	fmap = (=<<) . (return .)

instance Applicative (State s) where
	pure = return
	mf <*> mx = [ f x | f <- mf, x <- mx ]

instance Monad (State s) where
	return = State . (,)
	State m >>= f = State $ \s -> let (x, s') = m s in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put x = State $ \_ -> ((), x)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f

madd :: Integer -> State Integer ()
madd x = modify (+ x)

example :: State Integer Integer
example = do
	madd $ 3 * 4
	madd $ 2 * 5
	(* 7) <$> get

data Operation
	= E Expression
	| ErasePreLine
	deriving Show

data Expression
	= Expression :+: Expression
	| Expression :-: Expression
	| Expression :*: Expression
	| I Integer
	deriving Show

type Result = (Operation, Maybe Integer)

withOp :: String -> String -> String -> String
withOp op e1 e2 = "(" ++ e1 ++ " " ++ op ++ " " ++ e2 ++ ")"

showExp :: Expression -> String
showExp (e1 :+: e2) = withOp "+" (showExp e1) (showExp e2)
showExp (e1 :-: e2) = withOp "-" (showExp e1) (showExp e2)
showExp (e1 :*: e2) = withOp "*" (showExp e1) (showExp e2)
showExp (I n) = show n

evaluate :: Expression -> Integer
evaluate (o1 :+: o2) = evaluate o1 + evaluate o2
evaluate (o1 :-: o2) = evaluate o1 - evaluate o2
evaluate (o1 :*: o2) = evaluate o1 * evaluate o2
evaluate (I n) = n

operate :: Operation -> State [String] Result
operate o@(E e) = do
	modify ((showExp e ++ " = " ++ show r) :)
	return (o, Just r)
	where r = evaluate e
operate o@ErasePreLine = do
	modify (\ls -> if null ls then [] else tail ls)
	return (o, Nothing)

ltraverse :: Applicative f => (a -> f b) -> [a] -> f [b]
ltraverse f (x : xs) = (:) <$> f x <*> ltraverse f xs
ltraverse _ _ = pure []

operateAll :: [Operation] -> State [String] [Result]
operateAll = ltraverse operate

sampleOperation :: [Operation]
sampleOperation = [
	E $ I 3 :+: I 5,
	E $ I 8 :*: I 9,
	E $ I 7 :-: I 5,
	ErasePreLine,
	E $ I 3 :*: I 5 ]

grd :: Bool -> Maybe ()
grd = bool Nothing (Just ())

lfor :: Applicative f => [a] -> (a -> f b) -> f [b]
lfor = flip ltraverse
