type Id = Either Int String

name :: Id -> [(Id, String)] -> Maybe String
name = lookup

users :: [(Id, String)]
users = [
	(Right "yoshio", "Yoshio Yamada"),
	(Right "yoshio2", "Yoshio Yamada"),
	(Left 4492, "Tatsuya Yamashiro"),
	(Right "keiko", "Keiko Koike"),
	(Left 8855, "Satoru Hananakajima") ]
