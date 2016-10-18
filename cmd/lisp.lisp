"Definitions"

(defmacro defun (n a b)
	`(def (,n)
		((lambda ,a ,b))))

(defmacro defvar (n b)
	`(def (,n)
		(,b)))

"Variable setting"

(defmacro setvar (n b)
	`(set (,n)
		(,b)))

(defmacro setlvar (n b)
	`(setl (,n)
		(,b)))

"Control flow"

(defmacro when (x &rest b)
	`(if ,x
		(progn ~b)))

(defmacro unless (x &rest b)
	`(if (not ,x)
		(progn ~b)))

(defmacro for (s e i b)
	(progn
		(def (_l _e _i) ((gensym) (gensym) (gensym)))
		`(progn
			(def (,_l ,_e ,_i) (,s ,e ,i))
			(while (lt ,_l ,_e)
				,b
				(setlvar ,_l (add ,_l ,_i))))))

"Gather operations"

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

"Comparision"

(defun gte (x y)
	(not (lt x y)))

(defun lte (x y)
	(or (lt x y) (eq x y)))

(defun gt (x y)
	(not (or (lt x y) (eq x y))))

(defun eql (x y)
	(eq (str x) (str y)))

"Math functions"

(defun squared (x)
	(mul x x))

(defun cubed (x)
	(mul x x x))

(defun divmod (x y)
	(list (div x y) (mod x y)))

"Utilities"

(defun print_map (m)
	(progn
		(map print m)
		t))

(defun print_env (l e)
	(progn
		(print "**" l "**")
		(map print_map e)
		t))

(defun prin_num (n p c)
	(progn
		(defvar s (str n))
		(defvar l (length s))
		(while (lt l p)
			(prin c)
			(setlvar l (add l 1)))
		(prin s)))

"Some test code"

(defun fq (x y)
	(mod (mul (cubed x) (squared y)) 10))

(defun fxy (f w h)
	(progn
		(def (x y) (1 1))
		(until (lt h y)
			(setlvar x 1)
			(until (lt w x)
				(prin_num (f x y) 4 ".")
				(setlvar x (add x 1)))
			(setlvar y (add y 1))
			(print))))

(defun repeat_fxy (l)
	(progn
		(defvar c 0)
		(while (lt c l)
			(fxy fq 10 20)
			(setlvar c (add c 1)))))
