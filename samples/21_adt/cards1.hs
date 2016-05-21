type RGB = (Int, Int, Int)

data Suit = Spade | Heart | Diamond | Club deriving Show

data Color = Black | Red

rgb :: Suit -> RGB
rgb = toRGB . color

color :: Suit -> Color
color Spade = Black
color Heart = Red
color Diamond = Red
color Club = Black

toRGB :: Color -> RGB
toRGB Black = (0, 0, 0)
toRGB Red = (0xff, 0, 0)
