"Definitions"

(defmacro defun (n a &rest b)
	`(defq ,n (lambda ,a ~b)))

"Scopes"

(defmacro let (l &rest b)
	`((lambda ,(map (lambda (x) (elem 0 x)) l) ~b) ~(map (lambda (x) (elem 1 x)) l)))

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
			(defq _x (gensym))
			`(progn
				(defq ,_x ,x)
				(if ,_x ,_x (or ~b))))))

(defmacro and (x &rest b)
	(if (eq 0 (length b)) x
		`(if ,x (and ~b) nil)))

(defmacro for (s e i b)
	(progn
		(defq _l (gensym) _e (gensym) _i (gensym))
		`(progn
			(defq ,_l ,s ,_e ,e ,_i ,i)
			(while (lt ,_l ,_e)
				,b
				(setq ,_l (add ,_l ,_i))))))

"Map/Reduce operations"

(defmacro zip (&rest l)
 	`(map list ~l))

(defmacro merge (&rest l)
	`(reduce cat (zip ~l)))

"Comparison"

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

(defun inc (x)
	(add x 1))

(defun dec (x)
	(sub x 1))

(defun squared (x)
	(mul x x))

(defun cubed (x)
	(mul x x x))

(defun divmod (x y)
	(list (div x y) (mod x y)))

"Streams"

(defun each-line (f b)
	(progn
		(defq s (file-stream f) l t)
		(while (setq l (read-line s))
			(b l))))

(defun print-file (f)
	(each-line f print))

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
		(defq s (str n) l (length s))
		(while (lt l p)
			(prin c)
			(setq l (add l 1)))
		(prin s)))

"Some test code"

(defun fq (x y)
	(mod (mul (cubed x) (squared y)) 10))

(defun fxy (f w h)
	(progn
		(defq x 1 y 1)
		(until (lt h y)
			(setq x 1)
			(until (lt w x)
				(prin-num (f x y) 4 ".")
				(setq x (add x 1)))
			(setq y (add y 1))
			(print))))

(defun repeat-fxy (l)
	(progn
		(defq c 0)
		(while (lt c l)
			(fxy fq 10 20)
			(setq c (add c 1)))))
