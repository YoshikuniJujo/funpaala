import Book

main :: IO ()
main = interact $ fromBooklist . read
