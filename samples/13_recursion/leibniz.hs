estPi :: Int -> Double
-- estPi n = 4 * estPi4 (fromIntegral n)
estPi = (4 *) . estPi4 . fromIntegral

estPi4 0 = 1
estPi4 n = estPi4 (n - 1) + (- 1) ** n / (2 * n + 1)
