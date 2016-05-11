sumN :: Integer -> Integer
sumN n = sum [0 .. n]

sum3N :: Integer -> Integer
sum3N n = sum $ filter ((== 0) . (`mod` 3)) [0 .. 3 * n]
