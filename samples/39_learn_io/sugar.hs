sweet :: IO ()
sweet = do
	inp <- getLine
	putStrLn $ reverse inp

bitter :: IO ()
bitter = putStrLn . reverse =<< getLine
