import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import System.Random (randomRIO)
import System.Environment (getArgs)

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
	a1 : _ <- getArgs
	let	mx_ = fromMaybe 10 $ readMaybe a1
		mx = if mx_ < 1 then 10 else mx_
	putStrLn $ "Guess the Number (1 - " ++ show mx ++ ")"
	n <- randomRIO (1, mx)
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
