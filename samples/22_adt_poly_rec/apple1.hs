data Apple = Leaf | Flower | Fruit | Branch Apple Apple deriving Show

type Basket = [Apple]

price1 :: Apple -> Int
price1 Leaf = 50
price1 Flower = 80
price1 Fruit = 100
price1 (Branch a1 a2) = 20 + price1 a1 + price1 a2

price :: Basket -> Int
price = sum . map price1

dfs :: Apple -> [Apple]
dfs (Branch a1 a2) = dfs a1 ++ dfs a2
dfs a = [a]
