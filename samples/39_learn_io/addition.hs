import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

getInteger :: IO Integer
getInteger = do
	inp <- getLine
	return . fromMaybe 0 $ readMaybe inp

main :: IO ()
main = do
	putStrLn "Please input two number for add"
	n <- getInteger
	m <- getInteger
	putStrLn $ show (n + m)
