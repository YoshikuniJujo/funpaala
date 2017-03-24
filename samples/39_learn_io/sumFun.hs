import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

getInteger :: IO Integer
getInteger = fromMaybe 0 . readMaybe <$> getLine

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile m = do
	mx <- m
	case mx of
		Just x -> (x :) <$> doWhile m
		Nothing -> return []

main :: IO ()
main = do
	putStrLn "Please input numbers (end with -1)"
	ns <- doWhile $ do
		n <- getInteger
		return $ if n < 0 then Nothing else Just n
	putStrLn . show $ sum ns
