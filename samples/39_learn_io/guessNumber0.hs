import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Random (randomRIO)

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
	putStrLn "Guess the Number (1 - 10)"
	n <- randomRIO (1, 10)
	_ <- doWhile $ do
		g <- getInteger
		case g `compare` n of
			EQ -> do
				putStrLn "You win!"
				return Nothing
			LT -> do
				putStrLn $ "Your guess, " ++ show g ++
					", is too low."
				return $ Just ()
			GT -> do
				putStrLn $ "Your guess, " ++ show g ++
					", is too high."
				return $ Just ()
	return ()
