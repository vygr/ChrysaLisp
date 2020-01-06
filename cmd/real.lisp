;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: real [options]
	options:
		-h --help: this help info.
	+m ++mantisa 3-32: default 32.")
(("+m" "++mantisa")
	,(bind-fun (lambda (o f) (setq mbits (to-num f)))))
))

(defun-bind unpack-real (n)
	(list (>>> n mbits) (>>> (<< n ebits) ebits)))

(defun-bind pack-real (e m)
	(+ (<< e mbits) (>> (<< m ebits) ebits)))

(defun-bind norm-real (e m)
	(cond
		((> m 0)
			;positive
			(while (>= m (<< 1 (- mbits 1)))
				(setq m (>>> m 1) e (inc e)))
			(while (< m (<< 1 (- mbits 2)))
				(setq m (<< m 1) e (dec e))))
		((< m 0)
			;negative
			(while (<= m (<< -1 (- mbits 1)))
				(setq m (>>> m 1) e (inc e)))
			(while (> m (<< -1 (- mbits 2)))
				(setq m (<< m 1) e (dec e))))
		(t	;zero
			(setq e 0)))
	(list e m))

(defun-bind add-real (n1 n2)
	;unpack
	(bind '(e1 m1) (unpack-real n1))
	(bind '(e2 m2) (unpack-real n2))
	(cond
		((= (defq s (- e1 e2)) 0)
			;exponents the same
			(setq m1 (+ m1 m2)))
		((> s 0)
			;need to align m2
			(setq m1 (+ m1 (>>> m2 s))))
		(t
			;need to align m1
			(setq m1 (+ (>>> m1 (neg s)) m2) e1 e2)))
	;normalise and pack
	(apply pack-real (norm-real e1 m1)))

(defun-bind sub-real (n1 n2)
	;unpack
	(bind '(e1 m1) (unpack-real n1))
	(bind '(e2 m2) (unpack-real n2))
	(cond
		((= (defq s (- e1 e2)) 0)
			;exponents the same
			(setq m1 (- m1 m2)))
		((> s 0)
			;need to align m2
			(setq m1 (- m1 (>>> m2 s))))
		(t
			;need to align m1
			(setq m1 (- (>>> m1 (neg s)) m2) e1 e2)))
	;normalise and pack
	(apply pack-real (norm-real e1 m1)))

(defun-bind mul-real (n1 n2)
	;unpack
	(bind '(e1 m1) (unpack-real n1))
	(bind '(e2 m2) (unpack-real n2))
	;normalise and pack
	(apply pack-real (norm-real (+ e1 e2 (+ (neg mbits) 1)) (* m1 m2))))

(defun-bind div-real (n1 n2)
	;unpack
	(bind '(e1 m1) (unpack-real n1))
	(bind '(e2 m2) (unpack-real n2))
	;normalise and pack
	(apply pack-real (norm-real (- e1 e2 (- ebits mbits -1)) (/ (<< m1 ebits) m2))))

(defun-bind int-real (n)
	(apply pack-real (norm-real (- mbits 1) n)))

(defun-bind fixed-real (n)
	(apply pack-real (norm-real (- mbits fp_shift 1) n)))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq mbits 32 args (options slave usage)))
	(defq ebits (- 64 mbits)
		n1 (fixed-real 2.573) n2 (fixed-real 1.091) n3 (int-real 0) n4 (div-real (int-real 1024) (int-real 4)))
	(print "Num: " (array n1 n2 n3 n4))
	(print "Add: " (array (add-real n2 n1) (add-real n1 n2)))
	(print "Sub: " (array (sub-real n1 n2) (sub-real n2 n1) (sub-real n1 n1)))
	(print "Mul: " (array (mul-real n2 n1) (mul-real n2 n1) (mul-real n1 n3) (mul-real n2 n4)))

	(print "Result Mul: " (array (add-real n1 (mul-real n2 n4))))
	(each (lambda (_)
		(setq n1 (add-real n1 n2))) (range 0 256))
	(print "Result Acc: " (array n1))
)
