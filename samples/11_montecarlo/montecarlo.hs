import System.Random

randomXYs :: Int -> ([Double], [Double])
randomXYs n = let (g, g') = split $ mkStdGen n in
	(randomRs (-1, 1) g, randomRs (-1, 1) g')

points :: Int -> [(Double, Double)]
points n = let (xs, ys) = randomXYs n in zip xs ys

inCircle :: (Double, Double) -> Bool
inCircle (x, y) = x ^ 2 + y ^ 2 <= 1

inCirclePoints :: Int -> Int -> [(Double, Double)]
inCirclePoints g n = filter inCircle . take n $ points g

guessPi :: Int -> Int -> Double
guessPi g n = 4 * fromIntegral (length $ inCirclePoints g n) / fromIntegral n
