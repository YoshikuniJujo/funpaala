data Apple = Leaf | Flower | Fruit deriving Show

data Tree a = Branch (Tree a) (Tree a) | Atom a deriving Show

type Basket = [Apple]

price1 :: Apple -> Int
price1 Leaf = 50
price1 Flower = 80
price1 Fruit = 100

priceT :: Tree Apple -> Int
priceT (Atom a) = price1 a
priceT (Branch l r) = 20 + priceT l + priceT r

price :: Basket -> Int
price = sum . map price1

dfs :: Tree a -> [a]
dfs (Atom x) = [x]
dfs (Branch l r) = dfs l ++ dfs r
