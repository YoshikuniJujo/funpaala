import IOMcn

message :: Bool -> IOMcn String ()
message True = arr reverse >>> putLn
message False = putLn

sayHello :: Bool -> (IOMcn String (), String)
sayHello b = (message b, "hello")

greeting :: IOMcn () ()
greeting = isEven >>> arr sayHello >>> app

outArg :: IOMcn a b -> a -> IOMcn () b
outArg m x = arr (const x) >>> m

inArg :: (a -> IOMcn () b) -> IOMcn a b
inArg f = arr (\x -> (f x, ())) >>> app

some :: IO ()
some = do
	str <- getLine
	putStrLn str
