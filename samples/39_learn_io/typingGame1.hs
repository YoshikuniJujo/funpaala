import Data.Traversable (for)
import Data.Maybe (catMaybes)
import Data.List (genericLength)
import Data.Char (isSpace)
import Data.Time (getCurrentTime, diffUTCTime)
import Text.Read (readMaybe)
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
	let rslt = sum ps `div` genericLength ps
	print rslt
	ranking "practice/ranking.txt" rslt

type Record = (String, Integer)

record :: String -> Maybe Record
record str = let n : p : _ = words str in (,) n <$> readMaybe p

fromRecord :: Record -> String
fromRecord (n, p) = n ++ " " ++ show p

insertRecord :: Integer -> [Record] -> [(Maybe String, Integer)]
insertRecord px rrs@((nr, pr) : rs)
	| px >= pr = (Nothing, px) : map (\(n, p) -> (Just n, p)) rrs
	| otherwise = (Just nr, pr) : insertRecord px rs
insertRecord px [] = [(Nothing, px)]

yourName :: [(Maybe String, Integer)] -> IO [Record]
yourName ((Just n, p) : rs) = ((n, p) :) <$> yourName rs
yourName ((Nothing, p) : rs) = do
	putStrLn "What's your name?"
	n <- getLine
	((filter (not . isSpace) n, p) :) <$> yourName rs
yourName [] = return []

ranking :: FilePath -> Integer -> IO ()
ranking fp p = do
	rs <- catMaybes . map record <$> fileLines fp
	rs' <- yourName . take 10 $ insertRecord p rs
	writeFile fp . unlines $ map fromRecord rs'
