import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

getInteger :: IO Integer
getInteger = fromMaybe 0 . readMaybe <$> getLine

main :: IO ()
main = do
	putStrLn "Please input numbers (end with -1)"
	ns <- getIntegers
	putStrLn . show $ sum ns

getIntegers :: IO [Integer]
getIntegers = do
	n <- getInteger
	if n < 0 then return [] else (n :) <$> getIntegers
