"C-Script compiler !"

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
	(defq k "")
	(while (and c (p c))
		(setq k (cat k (char c)))
		(setq c (read-char s)))
	(list k c))

(defun read-white-space (s c)
	(read-token s c is-white-space))

(defun read-num (s c)
	(read-token s c is-num))

(defun read-alpha-num (s c)
	(read-token s c is-alpha-num))

(defun read-ident (s c)
	(read-token s c is-ident))

(defun read-group (s c)
	(defq k (read-token s (read-char s) (lambda (x) (not (is-group-close x)))))
	(list (elem 0 k) (read-char s)))

(defun read-file (f)
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
		(if (not (eql "" k)) (print k))))

(read-file "cmd/lisp.nasm")
