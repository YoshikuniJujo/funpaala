import Control.Applicative (Alternative(..))
import Data.Maybe (listToMaybe, fromJust)
import Data.Char (isDigit, isSpace, digitToInt)

newtype Parse a = Parse { runParse :: String ->[(a, String)] }

succeed :: a -> Parse a
succeed v = Parse $ \inp ->[(v, inp)]

check :: (Char -> Bool) -> Parse Char
check p = Parse $ \inp -> case inp of
	c : cs | p c -> [(c, cs)]
	_ -> []

char :: Char -> Parse Char
char = check . (==)

instance Functor Parse where
	fmap f (Parse p) = Parse $ \inp -> [ (f x, r) | (x, r) <- p inp ]

instance Applicative Parse where
	Parse pf <*> Parse px = Parse $ \inp ->
		[ (f x, r') | (f, r) <- pf inp, (x, r') <- px r ]
	pure = succeed

instance Alternative Parse where
	Parse p1 <|> Parse p2 = Parse $ \inp -> p1 inp ++ p2 inp
	empty = Parse $ \_ -> []

(>*>) :: Applicative f => f a -> f b -> f (a, b)
fx >*> fy = (,) <$> fx <*> fy

eof :: Parse ()
eof = Parse $ \inp -> case inp of
	"" -> [((), "")]
	_ -> []

number :: Parse Integer
number = read <$> some (check isDigit)

parse :: Parse a -> String -> Maybe a
parse p = listToMaybe . map fst . ((p <* eof) `runParse`)

spaces1 :: Parse ()
spaces1 = () <$ some (check isSpace)

spaces :: Parse ()
spaces = () <$ many (check isSpace)

comma :: Parse ()
comma = () <$ (spaces >*> char ',' >*> spaces)

sep :: Parse ()
sep = spaces1 <|> comma

numbers :: Parse [Integer]
-- numbers = uncurry (:) <$> (number >*> many (sep *> number))
numbers = (:) <$> number <*> many (sep *> number)

type Op = Integer -> Integer -> Integer

op, ad, sb, ml, dv :: Parse Op
op = ad <|> sb <|> ml <|> dv
ad = (+) <$ char '+'
sb = (-) <$ char '-'
ml = (*) <$ char '*'
dv = div <$ char '/'

expr :: Parse Integer
expr = (\x o y -> x `o` y) <$> term <*> op <*> term

term :: Parse Integer
term = number <|> (char '(' *> expr <* char ')')

calc :: String -> Maybe Integer
calc = parse expr
