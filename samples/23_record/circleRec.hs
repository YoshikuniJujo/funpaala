data Circle = Circle { centerX :: Double, centerY :: Double, radius :: Double }
	deriving Show

circle1 :: Circle
circle1 = Circle { centerX = 10, centerY = 15, radius = 7 }

area :: Circle -> Double
area Circle { radius = r } = r ^ 2 * pi

inside :: Circle -> (Double, Double) -> Bool
inside Circle { centerX = cx, centerY = cy, radius = r } (x, y) =
	(x - cx) ^ 2 + (y - cy) ^ 2 <= r ^ 2

moveH :: Circle -> Double -> Circle
moveH c@Circle { centerX = cx } dx = c { centerX = cx + dx }
