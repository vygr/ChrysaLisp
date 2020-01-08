;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: real [options]
	options:
		-h --help: this help info.
	+m ++mantisa 2-31: default 31.")
(("+m" "++mantisa")
	,(bind-fun (lambda (o f) (setq mbits (to-num f)))))
))

(defun-bind real-unpack (n)
	(list (>>> (<< n (- 64 ebits)) (- 64 ebits)) (>>> n ebits)))

(defun-bind real-pack (e m)
	(+ (<< m ebits) (>> (<< e (- 64 ebits)) (- 64 ebits))))

(defun-bind real-norm (e m)
	(cond
		((> m 0)
			;positive
			(while (>= m (<< 1 mbits))
				(setq m (>>> m 1) e (inc e)))
			(while (< m (<< 1 (dec mbits)))
				(setq m (<< m 1) e (dec e))))
		((< m 0)
			;negative
			(while (<= m (<< -1 mbits))
				(setq m (>>> m 1) e (inc e)))
			(while (> m (<< -1 (dec mbits)))
				(setq m (<< m 1) e (dec e))))
		(t	;zero
			(setq e 0)))
	(list e m))

(defun-bind real-add (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	(cond
		;exponents the same
		((= (defq s (- e1 e2)) 0) (setq m1 (+ m1 m2)))
		;need to align m2
		((> s 0) (setq m1 (+ m1 (>>> m2 s))))
		;need to align m1
		(t (setq m1 (+ (>>> m1 (neg s)) m2) e1 e2)))
	;normalise and pack
	(apply real-pack (real-norm e1 m1)))

(defun-bind real-sub (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	(cond
		;exponents the same
		((= (defq s (- e1 e2)) 0) (setq m1 (- m1 m2)))
		;need to align m2
		((> s 0) (setq m1 (- m1 (>>> m2 s))))
		;need to align m1
		(t (setq m1 (- (>>> m1 (neg s)) m2) e1 e2)))
	;normalise and pack
	(apply real-pack (real-norm e1 m1)))

(defun-bind real-mul (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	;normalise and pack
	(apply real-pack (real-norm (+ e1 e2 (neg mbits)) (* m1 m2))))

(defun-bind real-div (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	;normalise and pack
	(apply real-pack (real-norm (- e1 e2 (inc (* (- 31 mbits) 2))) (/ (<< m1 (- 63 mbits)) m2))))

(defun-bind int-to-real (n)
	(apply real-pack (real-norm mbits n)))

(defun-bind fixed-to-real (n)
	(apply real-pack (real-norm (- mbits fp_shift) n)))

(defun-bind real-to-int (n)
	;unpack
	(bind '(e m) (real-unpack n))
	;norm to int
	(cond
		((= (setq e (- mbits e)) 0) m)
		((> e 0) (>>> m e))
		(t (<< m (neg e)))))

(defun-bind real-to-fixed (n)
	;unpack
	(bind '(e m) (real-unpack n))
	;norm to fixed
	(cond
		((= (setq e (- mbits fp_shift e)) 0) m)
		((> e 0) (>>> m e))
		(t (<< m (neg e)))))

(defun-bind fixed-to-str (n)
	(defq s "")
	(when (< n 0)
		(setq s "-" n (neg n)))
	(defq i (>> n fp_shift) f (fmul (- n (<< i fp_shift)) 100000))
	(cat s (str i) "." (pad f 5 "00000")))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq mbits 31 args (options slave usage)))
	(defq ebits (- 63 mbits))

	(defq n1 (fixed-to-real 2.571) n2 (fixed-to-real -45.599) n3 (int-to-real 0) n4 (real-div (int-to-real 1024) (int-to-real 4)))
	(print "Nums: " (array n1 n2 n3 n4))
	(print "Add: " (array (real-add n2 n1) (real-add n1 n2)))
	(print "Sub: " (array (real-sub n1 n2) (real-sub n2 n1) (real-sub n1 n1)))
	(print "Mul: " (array (real-mul n2 n1) (real-mul n2 n1) (real-mul n1 n3) (real-mul n2 n4)))

	(print "Result Mul: " (array (real-add n1 (real-mul n2 n4))))
	(each (lambda (_)
		(setq n1 (real-add n1 n2))) (range 0 256))
	(print "Result Acc: " (array n1) " " (fixed-to-str (real-to-fixed n1)))
)
