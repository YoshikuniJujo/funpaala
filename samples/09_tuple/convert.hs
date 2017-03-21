halve :: Double -> Double
halve = (/ 2)

seven :: Integer
seven = 7

convert :: (Double -> Double) -> Integer -> Double
convert f n = f $ fromIntegral n
