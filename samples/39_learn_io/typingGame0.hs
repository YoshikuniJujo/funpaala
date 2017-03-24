import Data.Traversable (for)
import Data.List (genericLength)
import Data.Time (getCurrentTime, diffUTCTime)
import System.IO (IOMode(..), withFile, hGetLine, hIsEOF)
import System.Environment (getArgs)

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile m = do
	mx <- m
	case mx of
		Just x -> (x :) <$> doWhile m
		Nothing -> return []

fileLines :: FilePath -> IO [String]
fileLines fp = withFile fp ReadMode $ \h -> doWhile $ do
	b <- hIsEOF h
	if b then return Nothing else Just <$> hGetLine h

getLineWithTime :: IO (String, Integer)
getLineWithTime = do
	t1 <- getCurrentTime
	s <- getLine
	t2 <- getCurrentTime
	return (s, ceiling $ t2 `diffUTCTime` t1)

countMiss :: String -> String -> Integer
countMiss (c : cs) (d : ds)
	| c == d = countMiss cs ds
	| otherwise = 1 + countMiss cs ds
countMiss cs [] = genericLength cs
countMiss [] ds = genericLength ds

point :: String -> String -> Integer -> Integer
point s0 s1 sc = genericLength s0 * 60 `div` (sc + countMiss s0 s1)

main :: IO ()
main = do
	fp : _ <- getArgs
	ls <- fileLines fp
	ps <- for ls $ \l -> do
		putStrLn l
		(s, sc) <- getLineWithTime
		let	p = point l s sc
		print p
		return p
	print $ sum ps `div` genericLength ps
