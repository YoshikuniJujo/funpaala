module Book (Book(..), booklist, fromBooklist) where

import Data.Maybe (mapMaybe)
import Data.List (find)
import Data.Tree (Tree(..))
import Nml

data Book = Book {
	title :: String,
	author :: String
	} deriving (Show, Read)

books1 :: String
books1 = "<books>" ++
	"<book>" ++
	"<title>The Old Man and the Sea</title>" ++
	"<author>Ernest Hemingway</author>" ++
	"</book>" ++
	"<book>" ++
	"<title>The Catcher in the Rye</title>" ++
	"<author>J. D. Salinger</author>" ++
	"</book>" ++
	"</books>"

booklistNml :: Nml -> Maybe [Book]
booklistNml (Node "books" bl) = Just $ mapMaybe book bl
booklistNml _ = Nothing

book :: Nml -> Maybe Book
book b@(Node "book" _) = case get "title" b of
	Just t -> fmap (Book t) (get "author" b)
	Nothing -> Nothing
book _ = Nothing

get :: String -> Nml -> Maybe String
get tg (Node _ cs) = case find ((== tg) . rootLabel) cs of
	Just (Node _ [Node v []]) -> Just v
	_ -> Nothing

booklist :: String -> Maybe [Book]
booklist = maybe Nothing booklistNml . nml

fromBooklist :: [Book] -> String
fromBooklist = fromNml . fromBooklistNml

fromBook :: Book -> Nml
fromBook b = Node "book" [
	Node "title" [Node (title b) []],
	Node "author" [Node (author b) []] ]

fromBooklistNml :: [Book] -> Nml
fromBooklistNml = Node "books" . map fromBook
