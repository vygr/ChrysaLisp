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

"Scopes"

(defmacro let (l b)
	(progn
		(def (s v)
			((map (lambda (x) (elem x 0)) l)
			(map (lambda (x) (elem x 1)) l)))
		`((lambda ,s ,b) ~v)))

"Control flow"

(defmacro if (x y &rest b)
	(cond ((eq 0 (length b)) `(cond (,x ,y)))
		(t `(cond (,x ,y) (t ~b)))))

(defmacro when (x &rest b)
	`(cond (,x ~b)))

(defmacro unless (x &rest b)
	`(cond ((not ,x) ~b)))

(defmacro until (x &rest b)
	`(while (not ,x) ~b))

(defmacro or (x &rest b)
	(if (eq 0 (length b)) x
		(progn
			(defvar _x (gensym))
			`(progn
				(defvar ,_x ,x)
				(if ,_x ,_x (or ~b))))))

(defmacro and (x &rest b)
	(if (eq 0 (length b)) x
		`(if ,x (and ~b) nil)))

(defmacro for (s e i b)
	(progn
		(def (_l _e _i) ((gensym) (gensym) (gensym)))
		`(progn
			(def (,_l ,_e ,_i) (,s ,e ,i))
			(while (lt ,_l ,_e)
				,b
				(setlvar ,_l (add ,_l ,_i))))))

"Map/Reduce operations"

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

"Comparision"

(defun ne (x y)
	(not (eq x y)))

(defun ge (x y)
	(not (lt x y)))

(defun le (x y)
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

(defun print-map (m)
	(progn
		(map print m)
		t))

(defun print-env (l e)
	(progn
		(print "**" l "**")
		(map print_map e)
		t))

(defun prin-num (n p c)
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

(defun repeat-fxy (l)
	(progn
		(defvar c 0)
		(while (lt c l)
			(fxy fq 10 20)
			(setlvar c (add c 1)))))

(defun each-line (f b)
	(progn
		(def (s l) ((file-stream f) t))
		(while (setlvar l (read-line s))
			(b l))))

(defun print-file (f)
	(each-line f print))
