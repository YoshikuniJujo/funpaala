import Book

main :: IO ()
main = interact $ maybe "" ((++ "\n") . show) . booklist
