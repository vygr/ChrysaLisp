(def	(squared cubed divmod)
	 	(
			(lambda (x) (mul x x))
			(lambda (x) (mul x x x))
			(lambda (x y) (list (div x y) (mod x y)))
		)
)

(def	(zip1 zip2 zip3 zip4)
		(
			(lambda (a) (map list a))
			(lambda (a b) (map list a b))
			(lambda (a b c) (map list a b c))
			(lambda (a b c d) (map list a b c d))
		)
)
