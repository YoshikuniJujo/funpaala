newtype State s a = State { runState :: s -> (a, s) }

retS :: a -> State s a
retS x = State $ \s -> (x, s)

bindS :: State s a -> (a -> State s b) -> State s b
State m `bindS` f = State $ \s -> let (x, s') = m s in runState (f x) s'

instance Functor (State s) where
	fmap f m = m `bindS` (retS . f)

instance Applicative (State s) where
	pure = retS
	mf <*> mx = mf `bindS` \f -> mx `bindS` \x -> retS $ f x

instance Monad (State s) where
	return = retS
	(>>=) = bindS

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

data Operation = E Expression | ErasePreLine deriving Show

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

operateAll :: [Operation] -> State [String] [Result]
operateAll (o : os) = (:) <$> operate o <*> operateAll os
operateAll [] = pure []

sampleOperation :: [Operation]
sampleOperation = [
	E $ I 3 :+: I 5,
	E $ I 8 :*: I 9,
	E $ I 7 :-: I 5,
	ErasePreLine,
	E $ I 3 :*: I 5 ]
