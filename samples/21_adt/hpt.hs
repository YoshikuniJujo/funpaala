type Human = (String, Int)

age :: Human -> String
age (n, a) = n ++ " is " ++ show a ++ " years old."

masuo :: Human
masuo = ("Masuo", 32)

type Product = (String, Int)

price :: Product -> String
price (n, p) = n ++ " is " ++ show p ++ " yen."

iphone6s :: Product
iphone6s = ("iPhone 6s", 99000)
