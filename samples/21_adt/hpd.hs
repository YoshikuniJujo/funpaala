data Human = Human String Int deriving Show

age :: Human -> String
age (Human n a) = n ++ " is " ++ show a ++ " years old."

masuo :: Human
masuo = Human "Masuo" 32

data Product = Product String Int deriving Show

price :: Product -> String
price (Product n p) = n ++ " is " ++ show p ++ " yen."

smartphone :: Product
smartphone = Product "Smartphone" 99000
