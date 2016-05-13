dist0 :: (Double, Double) -> Double
dist0 (x, y) = sqrt $ x ^ 2 + y ^ 2

inCircle :: (Double, Double) -> Double -> (Double, Double) -> Bool
inCircle (x0, y0) r (x, y) = (x - x0) ^ 2 + (y - y0) ^ 2 <= r ^ 2
