module Books (Book(..), booklist) where

import Data.Maybe
import Data.List
import Data.Tree
import Nml

data Book = Book {
	title :: String,
	author :: String }
	deriving Show

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
book b@(Node "book" _) = flip (maybe Nothing) (get "title" b) $ \t ->
	flip (maybe Nothing) (get "author" b) $ \a ->
		Just $ Book { title = t, author = a }
book _ = Nothing

get :: String -> Nml -> Maybe String
get tg (Node _ cs) = case find ((== tg) . rootLabel) cs of
	Just (Node _ [Node v []]) -> Just v
	_ -> Nothing

booklist :: String -> Maybe [Book]
booklist = maybe Nothing booklistNml . nml