import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

getInteger :: IO Integer
getInteger = fromMaybe 0 . readMaybe <$> getLine

main :: IO ()
main = do
	putStrLn "Please input two number for add"
	putStrLn . show =<< (+) <$> getInteger <*> getInteger
