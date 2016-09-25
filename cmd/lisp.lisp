(def	(squared cubed divmod)
	 	(
			(lambda (x) (mul x x))
			(lambda (x) (mul x x x))
			(lambda (x y) (list (div x y) (mod x y)))
		)
)
