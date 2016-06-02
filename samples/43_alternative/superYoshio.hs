import Control.Monad (foldM)

type Yoshio = (Integer, Integer)

data Event = E Enemy | I Item deriving Show

data Enemy = Ghost | Troll | Vampire deriving Show
data Item = Shell | Coin | Jewel | Star deriving Show

damage :: Enemy -> Integer
damage Ghost = 10
damage Troll = 50
damage Vampire = 100

score :: Item -> Integer
score Shell = 50
score Coin = 100
score Jewel = 500
score Star = 1000

stage1, stage2 :: [Event]
stage1 = [E Ghost, I Shell, I Star, I Coin, E Troll, I Coin, E Vampire]
stage2 = [E Vampire, E Vampire, I Star, I Star, I Star, I Coin, I Star]

mguard :: Bool -> Maybe ()
mguard True = Just ()
mguard _ = Nothing

mevent :: Yoshio -> Event -> Maybe Yoshio
mevent (hp, s) (E e) = do
	let hp' = hp - damage e
	mguard $ hp' > 0
	return (hp', s)
mevent (hp, s) (I i) = return (hp, s + score i)

mgame :: Yoshio -> [Event] -> Maybe Yoshio
mgame = foldM mevent

malt :: Maybe a -> Maybe a -> Maybe a
mx@(Just _) `malt` _ = mx
_ `malt` my = my

malts :: [Maybe a] -> Maybe a
malts (x : xs) = x `malt` malts xs
malts _ = Nothing

lguard :: Bool -> [()]
lguard True = [()]
lguard _ = []

levent :: Yoshio -> Event -> [Yoshio]
levent (hp, s) (E e) = do
	let hp' = hp - damage e
	lguard $ hp' > 0
	return (hp', s)
levent (hp, s) (I i) = return (hp, s + score i)

lgame :: Yoshio -> [Event] -> [Yoshio]
lgame = foldM levent

lalt :: [a] -> [a] -> [a]
lalt = (++)

lalts :: [[a]] -> [a]
lalts (x : xs) = x `lalt` lalts xs
lalts _ = []
