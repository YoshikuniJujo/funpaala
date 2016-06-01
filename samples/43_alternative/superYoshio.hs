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

-- mguard :: Boool -> Maybe
