safeSqrt x = sqrtBool (x >= 0) x

sqrtBool True x = Just (sqrt x)
sqrtBool False _ = Nothing
