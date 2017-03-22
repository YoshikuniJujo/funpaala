data LL a = Nil | Cons (LL a) a

instance Show a => Show (LL a) where
	show (Cons lst x) = "Cons (" ++ show lst ++ ") " ++ show x
	show Nil = "Nil"
