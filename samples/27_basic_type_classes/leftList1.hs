data LL a = Nil | Cons (LL a) a

instance Show a => Show (LL a) where
	showsPrec _ (Cons lst x) =
		("Cons (" ++) . showsPrec 11 lst . (") " ++) . showsPrec 11 x
	showsPrec _ Nil = ("Nil" ++)
