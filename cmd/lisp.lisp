(def (squared cubed divmod)
	((lambda (x) (mul x x))
	(lambda (x) (mul x x x))
	(lambda (x y) (list (div x y) (mod x y)))))

(def (zip1 zip2 zip3 zip4)
	((lambda (a) (map list a))
	(lambda (a b) (map list a b))
	(lambda (a b c) (map list a b c))
	(lambda (a b c d) (map list a b c d))))

(def (fq)
	((lambda (x y) (mod (mul (cubed x) (squared y)) 10))))

(def (f_xy)
	((lambda (f w h)
		(progn
			(def (y w h) (1 (add w 1) (add h 1)))
			(while (not (eq y h))
				(def (x) (1))
				(while (not (eq x w))
					(prin (f x y))
					(def (x) ((add x 1))))
				(def (y) ((add y 1)))
				(print))))))

(def (penv)
	((lambda ()
		(progn
			(map print (env)))))
