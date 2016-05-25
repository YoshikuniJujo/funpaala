data LL a = Nil | Cons (LL a) a

instance Show a => Show (LL a) where
	showsPrec _ (Cons ll x) =
		("Cons (" ++) . showsPrec 11 ll . (") " ++) . showsPrec 11 x
	showsPrec _ _ = ("Nil" ++)
