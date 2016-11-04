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
	(defq _l (gensym) _e (gensym) _i (gensym))
	`(progn
		(defq ,_l ,s ,_e ,e ,_i ,i)
		(while (lt ,_l ,_e)
			,b
			(setq ,_l (add ,_l ,_i)))))

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
	(defq s (file-stream f) l t)
	(while (setq l (read-line s))
		(b l)))

(defun print-file (f)
	(each-line f print))

"Utilities"

(defmacro ascii (c)
	(code c))

(defun prin-space (x)
	(prin x " ") t)

(defun prin-map (m)
	(map prin-space m))

(defun print-map (m)
	(map print m) t)

(defun print-env (l e)
	(print "**" l "**")
	(map print_map e) t)

(defun prin-num (n p c)
	(defq s (str n) l (length s))
	(while (lt l p)
		(prin c)
		(setq l (add l 1)))
	(prin s))

"VP Assembler"

(defmacro byte (x)
	`(bit-and ,x 0xff))

(defmacro short (x)
	`(bit-and ,x 0xffff))

(defun emit-byte (x)
	(setq emit-buffer (cat emit-buffer (list (byte x)))))

(defun emit-bytes (&rest b)
	(map emit-byte b))

(defun emit-short (x)
	(emit-bytes
		(bit-shr x 8) x))

(defun emit-int (x)
	(emit-bytes
		(bit-shr x 24) (bit-shr x 16) (bit-shr x 8) x))

(defun emit-long (x)
	(emit-bytes
		(bit-shr x 56) (bit-shr x 48) (bit-shr x 40) (bit-shr x 32)
		(bit-shr x 24) (bit-shr x 16) (bit-shr x 8) x))

(defun print-emit-buffer (c)
	(defq i 0)
	(while (lt i (length emit-buffer))
		(if (eq (mod i c) 0)
			(progn
				(prin-num i 4 "0") (prin " -> ")))
		(prin-num (elem i emit-buffer) 3 "0") (prin " ")
		(setq i (inc i))
		(if (eq (mod i c) 0)
			(print))))

(defq r0 0 r1 1 r2 2 r3 3 r4 4 r5 5 r6 6 r7 7 r8 8
	r9 9 r10 10 r11 11 r12 12 r13 3 r14 14 r15 15)

(defun vp-add-cr (c x)
	(emit-bytes 23 x)
	(emit-long c))

(defun vp-add-rr (x y)
	(emit-bytes 24 x y))

(defq emit-buffer '())

(vp-add-cr 3456 r0)
(vp-add-cr 345623 r1)
(vp-add-rr r3 r5)

(print-emit-buffer 16)
