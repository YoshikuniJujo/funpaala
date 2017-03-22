import qualified Data.Set as S

class IntegerStorage s where
	empty :: s
	store :: Integer -> s -> s
	derive :: s -> Maybe (Integer, s)

stored :: IntegerStorage s => s
stored = let
	s1 = store 8 empty
	s2 = store 5 s1
	s3 = store 2 s2
	s4 = store 10 s3
	s5 = store 3 s4 in
	s5

derive3 :: IntegerStorage s => s -> Maybe [Integer]
derive3 s = case derive s of
	Just (l, s1) -> case derive s1 of
		Just (m, s2) -> case derive s2 of
			Just (n, s3) -> Just [l, m, n]
			Nothing -> Nothing
		Nothing -> Nothing
	Nothing -> Nothing

newtype ListInteger = LI [Integer] deriving Show

instance IntegerStorage ListInteger where
	empty = LI []
	store n (LI s) = LI $ n : s
	derive (LI (n : s)) = Just (n, LI s)
	derive (LI []) = Nothing

newtype SetInteger = SI (S.Set Integer) deriving Show

instance IntegerStorage SetInteger where
	empty = SI S.empty
	store n (SI s) = SI $ S.insert n s
	derive (SI s)
		| S.null s = Nothing
		| otherwise = Just (n, SI s')
		where (n, s') = S.deleteFindMin s

data QueueInteger = QI [Integer] [Integer] deriving Show

instance IntegerStorage QueueInteger where
	empty = QI [] []
	store n (QI f r) = QI f (n : r)
	derive (QI (n : f) r) = Just (n, QI f r)
	derive (QI [] r@(_ : _)) = derive $ QI (reverse r) []
	derive (QI [] []) = Nothing

data OddEven = OE [Integer] [Integer] deriving Show

instance IntegerStorage OddEven where
	empty = OE [] []
	store n (OE os es)
		| odd n = OE (n : os) es
		| otherwise = OE os (n : es)
	derive (OE (o : os) es) = Just (o, OE os es)
	derive (OE [] (e : es)) = Just (e, OE [] es)
	derive (OE [] []) = Nothing
