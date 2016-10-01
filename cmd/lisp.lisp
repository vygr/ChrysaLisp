(def (print_env)
	((lambda (l e)
		(progn
			(print '<= l '=>)
			(map print e)
			t))))

(def (squared cubed divmod)
	((lambda (x)
		(progn
			(print_env 'squared_env (env))
			(mul x x)))
	(lambda (x)
		(progn
			(print_env 'cubed_env (env))
			(mul x x x)))
	(lambda (x y)
		(progn
			(print_env 'divmod_env (env))
			(list (div x y) (mod x y))))))

(def (zip1 zip2 zip3 zip4)
	((lambda (a) (map list a))
	(lambda (a b) (map list a b))
	(lambda (a b c) (map list a b c))
	(lambda (a b c d) (map list a b c d))))

(def (fq)
	((lambda (x y)
	 	(progn
			(print_env 'fq_env (env))
			(mod (mul (cubed x) (squared y)) 10)))))

(def (f_xy)
	((lambda (f w h)
		(progn
			(def (y w h) (1 (add w 1) (add h 1)))
			(print_env 'f_xy_env (env))
			(until (eq y h)
				(def (x) (1))
				(until (eq x w)
					(prin (f x y))
					(set (x) ((add x 1))))
				(set (y) ((add y 1)))
				(print))))))
