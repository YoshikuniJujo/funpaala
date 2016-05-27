{-# LANGUAGE TupleSections #-}

import Data.Char (chr, ord)

addUnitArg :: a -> (() -> a)
addUnitArg v = \_ -> v

delUnitArg :: (() -> a) -> a
delUnitArg f = f ()

addUnitTup :: a -> (a, ())
addUnitTup = (, ())

delUnitTup :: (a, ()) -> a
delUnitTup = fst

eight :: (Integer -> a) -> a
eight f = f 8

valToFun :: a -> ((a -> b) -> b)
valToFun x = ($ x)

funToVal :: ((a -> a) -> a) -> a
funToVal = ($ id)

myChr :: (a -> Int) -> (a -> Char)
myChr f = \x -> chr $ f x

addArg :: (b -> c) -> (a -> b) -> a -> c
addArg fun f x = fun (f x)

delArg :: ((a -> b) -> a -> c) -> b -> c
delArg fun x = fun (const x) undefined

myAdd :: (a -> Integer) -> Integer -> (a -> Integer)
myAdd f y x = f x + y

add :: Integer -> Integer -> Integer
add = (+)

addArg2 :: (b -> c -> d) -> (a -> b) -> c -> (a -> d)
addArg2 fun f y x = fun (f x) y

delArg2 :: ((a -> b) -> c -> (a -> d)) -> b -> c -> d
delArg2 f x y = f (const x) y undefined
