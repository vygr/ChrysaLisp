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

(defun equalp (x y)
	(eql (str x) (str y)))

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

"C-Script compiler !"

(defmacro ascii (c)
	(code c))

(defun is-num (c)
	(le (ascii "0") c (ascii "9")))

(defun is-alpha (c)
	(or (le (ascii "a") c (ascii "z")) (le (ascii "A") c (ascii "Z"))))

(defun is-alpha-num (c)
	(or (is-num c) (is-alpha c)))

(defun is-white-space (c)
	(lt c (ascii " ")))

(defun is-comment (c)
	(eq c (ascii ";")))

(defun is-group-open (c)
	(eq c (ascii "{")))

(defun is-group-close (c)
	(eq c (ascii "}")))

(defun is-ident (c)
	(or (is-alpha-num c) (eq c (ascii "_"))))

(defun read-token (s c p)
	(progn
		(defq k "")
		(while (and c (p c))
			(setq k (cat k (char c)))
			(setq c (read-char s)))
		(list k c)))

(defun read-white-space (s c)
	(read-token s c is-white-space))

(defun read-num (s c)
	(read-token s c is-num))

(defun read-alpha-num (s c)
	(read-token s c is-alpha-num))

(defun read-ident (s c)
	(read-token s c is-ident))

(defun read-group (s c)
	(progn
		(defq k (read-token s (read-char s) (lambda (x) (not (is-group-close x)))))
		(list (elem 0 k) (read-char s))))

(defun read-file (f)
	(progn
		(defq s (file-stream f)
			c (read-char s)
			k nil)
		(while c
			(setq k (read-white-space s c)
				c (elem 1 k)
				k (elem 0 k))
			(setq k (cond ((is-comment c) (list (cat (char c) (read-line s)) (read-char s)))
						((is-group-open c) (read-group s c))
						((is-num c) (read-num s c))
			 			((is-alpha c) (read-ident s c))
						(t (list "" (read-char s))))
				c (elem 1 k)
				k (elem 0 k))
			(if (not (eql "" k)) (print k)))))

(read-file "cmd/lisp.nasm")
