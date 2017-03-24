main :: IO ()
main = do
	putStrLn "What's your name?"
	n <- getLine
	putStrLn $ "Hello, " ++ n ++ "!"
