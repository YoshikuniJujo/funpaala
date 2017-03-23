import Data.Char (chr, ord)

addUnitArg :: a -> (() -> a)
addUnitArg v = \() -> v

delUnitArg :: (() -> a) -> a
delUnitArg f = f ()

addUnitTup :: a -> (a, ())
addUnitTup v = (v , ())

delUnitTup :: (a, ()) -> a
delUnitTup = fst

eight :: (Integer -> a) -> a
-- eight f = f 8
eight = ($ 8)

valToFun :: a -> ((a -> b) -> b)
valToFun x = ($ x)

funToVal :: ((a -> a) -> a) -> a
funToVal = ($ id)

myChr :: (a -> Int) -> (a -> Char)
myChr f = \x -> chr $ f x

addArg :: (b -> c) -> (a -> b) -> a -> c
addArg fun f x = fun (f x)

delArg :: ((() -> b) -> (() -> c)) -> (b -> c)
delArg fun x = fun (const x) ()

-- delArg :: ((b -> b) -> b -> c) -> b -> c
-- delArg fun = fun id

myAdd :: (a -> Integer) -> Integer -> (a -> Integer)
myAdd f y x = f x + y

add :: Integer -> Integer -> Integer
add = (+)

addArg2 :: (b -> c -> d) -> (a -> b) -> c -> a -> d
addArg2 fun f y x = fun (f x) y

delArg2 :: ((() -> b) -> c -> (() -> d)) -> b -> c -> d
delArg2 f x y = f (const x) y ()

-- delArg2 :: ((b -> b) -> c -> (b -> d)) -> b -> c -> d
-- delArg2 f = flip $ f id
