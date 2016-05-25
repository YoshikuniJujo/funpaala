data LL a = Nil | Cons (LL a) a

instance Show a => Show (LL a) where
	show (Cons ll x) = "Cons (" ++ show ll ++ ") " ++ show x
	show _ = "Nil"
