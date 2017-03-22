(.+.), (.*.) :: Int -> Int -> Int
(.+.) = (+)
(.*.) = (*)

-- infixl 6 .+., .-.
-- infixr 6 .+., .-.
infix 6 .+., .-.

(.-.) :: Int -> Int -> Int
(.-.) = (-)

data A = B :+: B deriving Show
data B = Int :*: Int deriving Show

infixl 6 :+:
infixl 7 :*:
