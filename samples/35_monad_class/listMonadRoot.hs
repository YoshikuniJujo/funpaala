root :: Double -> [Double]
root 0 = [0]
root x	| x < 0 = []
	| otherwise = [- sqrt x, sqrt x]

calc :: Double -> Double -> [Double]
calc a b = root . (+ b) =<< root a

grd :: Bool -> [()]
grd False = []
grd True = [()]

calc2 :: Double -> Double -> [Double]
calc2 a b = do
	x <- root a
	y <- root $ x + b
	grd $ y >= 0
	return y

calc3 :: Double -> Double -> [Double]
calc3 a b = [ y | x <- root a, y <- root $ x + b, y >= 0 ]
